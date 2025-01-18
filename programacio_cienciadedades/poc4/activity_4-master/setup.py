#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 07:24:05 2025

@author: albert
"""
from setuptools import setup, find_packages

setup(
    name="POC4",
    version="0.1.0",
    author="Albert",
    description="Data processing and analysis scripts for POC4",
    packages=find_packages(),
    install_requires=[
        "pandas",
        "faker",
        "matplotlib",
    ],
    },
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
