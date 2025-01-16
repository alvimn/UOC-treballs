#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 16 07:23:00 2025

@author: albert
"""

import setup
import anomize
import minutagg

df = setup.datapull()
df = anomize.name_surname(df)
df = minutagg.gruptimes(df)
