#!/bin/bash

oneWebApp() {
  cat webapp.json | sed 's/^/  /'
}

echo '{"webapps": ['
oneWebApp
i=0
while [[ $i -lt 1000 ]]; do
  echo ","
  oneWebApp
  i=$(($i + 1))
done
echo "]}"
