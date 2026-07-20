#!/usr/bin/env python3
"""Measure a small local visual policy independently from emulation."""

from __future__ import annotations

import argparse
import time

import torch
from torch import nn


class VisualPolicy(nn.Module):
    """A compact frame encoder with recurrent memory and actor/critic heads."""

    def __init__(self) -> None:
        super().__init__()
        self.encoder = nn.Sequential(
            nn.Conv2d(1, 32, kernel_size=8, stride=4),
            nn.SiLU(),
            nn.Conv2d(32, 64, kernel_size=4, stride=2),
            nn.SiLU(),
            nn.Conv2d(64, 64, kernel_size=3, stride=1),
            nn.SiLU(),
            nn.Flatten(),
            nn.Linear(64 * 14 * 16, 512),
            nn.SiLU(),
        )
        self.memory = nn.GRUCell(512, 512)
        self.policy = nn.Linear(512, 8)
        self.value = nn.Linear(512, 1)

    def forward(
        self,
        pixels: torch.Tensor,
        hidden: torch.Tensor,
    ) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        hidden = self.memory(self.encoder(pixels), hidden)
        return self.policy(hidden), self.value(hidden), hidden


def select_device(requested: str) -> str:
    if requested != "auto":
        return requested
    if torch.backends.mps.is_available():
        return "mps"
    if torch.cuda.is_available():
        return "cuda"
    return "cpu"


def synchronize(device: str) -> None:
    if device == "mps":
        torch.mps.synchronize()
    elif device.startswith("cuda"):
        torch.cuda.synchronize()


def measure(
    operation,
    device: str,
    warmup: int,
    iterations: int,
) -> float:
    for _ in range(warmup):
        operation()
    synchronize(device)
    start = time.perf_counter()
    for _ in range(iterations):
        operation()
    synchronize(device)
    return (time.perf_counter() - start) / iterations


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--batch", type=int, default=128)
    parser.add_argument("--iterations", type=int, default=30)
    parser.add_argument("--warmup", type=int, default=10)
    parser.add_argument("--device", default="auto")
    args = parser.parse_args()
    if args.batch <= 0 or args.iterations <= 0 or args.warmup < 0:
        parser.error("batch and iterations must be positive; warmup cannot be negative")
    return args


def main() -> None:
    args = parse_args()
    device = select_device(args.device)
    torch.manual_seed(0)

    model = VisualPolicy().to(device).eval()
    host_palette = torch.randint(
        0,
        4,
        (args.batch, 1, 144, 160),
        dtype=torch.uint8,
        device="cpu",
    )
    resident_pixels = host_palette.to(device=device, dtype=torch.float32) / 3.0
    resident_hidden = torch.zeros(args.batch, 512, device=device)
    upload_hidden = torch.zeros_like(resident_hidden)

    with torch.inference_mode():

        def resident_step() -> None:
            nonlocal resident_hidden
            _, _, resident_hidden = model(resident_pixels, resident_hidden)

        def upload_step() -> None:
            nonlocal upload_hidden
            pixels = host_palette.to(device=device, dtype=torch.float32) / 3.0
            _, _, upload_hidden = model(pixels, upload_hidden)

        resident_seconds = measure(
            resident_step,
            device,
            args.warmup,
            args.iterations,
        )
        upload_seconds = measure(
            upload_step,
            device,
            args.warmup,
            args.iterations,
        )

    parameters = sum(parameter.numel() for parameter in model.parameters())
    print("Nibble local visual-policy capacity benchmark")
    print(f"  torch: {torch.__version__}")
    print(f"  device: {device}")
    print(f"  parameters: {parameters:,}")
    print(f"  batch: {args.batch}")
    print(
        f"  device-resident: {resident_seconds * 1000:.3f} ms, "
        f"{args.batch / resident_seconds:.0f} observations/s"
    )
    print(
        f"  CPU uint8 upload: {upload_seconds * 1000:.3f} ms, "
        f"{args.batch / upload_seconds:.0f} observations/s"
    )


if __name__ == "__main__":
    main()
