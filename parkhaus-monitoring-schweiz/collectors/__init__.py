"""
Collectors module for Swiss parking monitoring system.
"""

from .base import BaseParkingCollector
from .luzern import LuzernCollector
from .basel import BaselCollector
from .stgallen import StGallenCollector
from .zurich import ZurichCollector

__all__ = [
    'BaseParkingCollector',
    'LuzernCollector',
    'BaselCollector',
    'StGallenCollector',
    'ZurichCollector',
]
