"""
Multi-city parking data collection orchestrator.

This script coordinates data collection from multiple Swiss cities with
PLS (Parkleitsystem) parking guidance systems.
"""

import argparse
import json
import os
import sys
import time
from datetime import datetime
from pathlib import Path

# Add collectors to path
sys.path.insert(0, os.path.dirname(__file__))

from collectors import (
    LuzernCollector,
    BaselCollector,
    StGallenCollector,
    ZurichCollector
)


# Collector class mapping
COLLECTOR_MAP = {
    "luzern.LuzernCollector": LuzernCollector,
    "basel.BaselCollector": BaselCollector,
    "stgallen.StGallenCollector": StGallenCollector,
    "zurich.ZurichCollector": ZurichCollector,
}


def load_config():
    """Load city configuration from config/cities.json."""
    config_path = Path(__file__).parent / "config" / "cities.json"
    try:
        with open(config_path, "r", encoding="utf-8") as f:
            return json.load(f)
    except FileNotFoundError:
        print(f"Error: Configuration file not found at {config_path}")
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in configuration file: {e}")
        sys.exit(1)


def create_collector(city_id, city_config, data_dir="data"):
    """
    Create a collector instance for a city.
    
    Args:
        city_id: City identifier
        city_config: City configuration dict
        data_dir: Base data directory
    
    Returns:
        BaseParkingCollector instance or None if collector not found
    """
    collector_class_name = city_config.get("collector")
    collector_class = COLLECTOR_MAP.get(collector_class_name)
    
    if not collector_class:
        print(f"Warning: Collector '{collector_class_name}' not found for {city_id}")
        return None
    
    return collector_class(
        city_id=city_id,
        city_name=city_config.get("name", city_id),
        api_url=city_config.get("api_url"),
        data_dir=data_dir
    )


def collect_city_data(city_id, config, data_dir="data"):
    """
    Collect data for a specific city.
    
    Args:
        city_id: City identifier
        config: Full configuration dict
        data_dir: Base data directory
    
    Returns:
        bool: True if successful, False otherwise
    """
    cities = config.get("cities", {})
    
    if city_id not in cities:
        print(f"Error: City '{city_id}' not found in configuration")
        return False
    
    city_config = cities[city_id]
    
    if not city_config.get("enabled", True):
        print(f"Info: City '{city_id}' is disabled in configuration")
        return False
    
    collector = create_collector(city_id, city_config, data_dir)
    if not collector:
        return False
    
    return collector.collect()


def collect_all_cities(config, data_dir="data"):
    """
    Collect data for all enabled cities.
    
    Args:
        config: Configuration dict
        data_dir: Base data directory
    
    Returns:
        dict: Results for each city {city_id: success_bool}
    """
    cities = config.get("cities", {})
    results = {}
    
    for city_id, city_config in cities.items():
        if not city_config.get("enabled", True):
            print(f"Skipping disabled city: {city_id}")
            continue
        
        print(f"\n{'='*60}")
        print(f"Collecting data for: {city_config.get('name', city_id)}")
        print(f"{'='*60}")
        
        success = collect_city_data(city_id, config, data_dir)
        results[city_id] = success
    
    return results


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Monitor parking data from Swiss cities with PLS systems"
    )
    parser.add_argument(
        "--once",
        action="store_true",
        help="Run once and exit"
    )
    parser.add_argument(
        "--city",
        type=str,
        help="Collect data for specific city only (e.g., 'luzern', 'basel')"
    )
    parser.add_argument(
        "--interval",
        type=int,
        default=900,
        help="Interval in seconds for continuous monitoring (default: 900 = 15 mins)"
    )
    parser.add_argument(
        "--data-dir",
        type=str,
        default="data",
        help="Base directory for data storage (default: 'data')"
    )
    
    args = parser.parse_args()
    
    # Load configuration
    config = load_config()
    
    print(f"Swiss Parking Monitor - Starting at {datetime.now()}")
    print(f"Data directory: {args.data_dir}")
    
    if args.once:
        # Single collection run
        if args.city:
            # Collect specific city
            success = collect_city_data(args.city, config, args.data_dir)
            sys.exit(0 if success else 1)
        else:
            # Collect all cities
            results = collect_all_cities(config, args.data_dir)
            
            # Print summary
            print(f"\n{'='*60}")
            print("Collection Summary:")
            print(f"{'='*60}")
            for city_id, success in results.items():
                status = "✓ Success" if success else "✗ Failed"
                print(f"{city_id:15} {status}")
            
            # Exit with error if any city failed
            all_success = all(results.values())
            sys.exit(0 if all_success else 1)
    else:
        # Continuous monitoring
        print(f"Running continuously with {args.interval}s interval")
        print("Press Ctrl+C to stop\n")
        
        try:
            while True:
                if args.city:
                    collect_city_data(args.city, config, args.data_dir)
                else:
                    collect_all_cities(config, args.data_dir)
                
                print(f"\nSleeping for {args.interval} seconds...")
                time.sleep(args.interval)
        except KeyboardInterrupt:
            print("\n\nStopped by user")
            sys.exit(0)


if __name__ == "__main__":
    main()
