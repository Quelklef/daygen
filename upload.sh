#!/bin/bash
set -e
rsync -rvutP index.{html,js} root@167.99.145.139:/var/www/daygen.maynards.site/
