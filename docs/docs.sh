#!/bin/bash
prep --help >docs/prep.1.man
txt2man docs/prep.1.man > docs/prep.1
man2html docs/prep.1 >docs/prep.1.html
