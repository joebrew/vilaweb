- Follow these instructions: https://www.digitalocean.com/community/tutorials/how-to-set-up-shiny-server-on-ubuntu-16-04

- Deploy an instance to ec2

### Installing some stuff

```
sudo apt-get update && sudo apt-get upgrade
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
sudo apt update

sudo apt install r-base


sudo apt-get install -y libcurl4-openssl-dev
sudo apt-get install -y libxml2-dev
sudo apt install default-jdk
export LD_LIBRARY_PATH=/usr/lib/jvm/java-11-openjdk-amd64/lib/server
sudo R CMD javareconf
```

IMPORTANT:
Change R package directory from user-based to system-wide. Find the below lines in your Renviron file and use your favorite text editor to swap the comment hashes to the configuration below.

sudo nano /usr/lib/R/etc/Renviron
Your Renviron file should look like this when you’re done.
#R_LIBS_USER=${R_LIBS_USER-‘~/R/x86_64-pc-linux-gnu-library/3.0’}
R_LIBS_USER=${R_LIBS_USER-‘~/Library/R/3.0/library’}

5. Check lib paths in R to make sure your package library changed correctly.
“/usr/local/lib/R/site-library” should be the first of the library paths.

R
.libPaths()
5. Make your new package lib readable for Shiny Server.

sudo chmod 777 /usr/lib/R/site-library

### Install shiny

sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""

### Install shiny servers
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.5.872-amd64.deb
md5sum shiny-server-1.5.5.872-amd64.deb
sudo apt-get update
sudo apt-get install gdebi-core
sudo gdebi shiny-server-1.5.5.872-amd64.deb
# Check that it's running on port 3838
sudo netstat -plunt | grep -i shiny
sudo ufw allow 3838

### Set up https


### Setting up https

```
sudo apt install nginx
sudo apt-get update
sudo apt-get install software-properties-common
sudo add-apt-repository universe
sudo add-apt-repository ppa:certbot/certbot
sudo apt-get update
sudo apt-get install certbot python-certbot-nginx
sudo certbot run --nginx --non-interactive --agree-tos -m joebrew@gmail.com --redirect -d datacat.cc
```

### Set up proxy, certificate, etc.

sudo nano /etc/nginx/nginx.conf

Copy the following into the http block of /etc/nginx/nginx.conf

```
http {
    ...
    # Map proxy settings for RStudio
    map $http_upgrade $connection_upgrade {
        default upgrade;
        '' close;
    }
}
```

Now create a new block
sudo nano /etc/nginx/sites-available/datacat.cc

In example.com’>/etc/nginx/sites-available/example.com

```
server {
   listen 80 default_server;
   listen [::]:80 default_server ipv6only=on;
   server_name datacat.cc www.datacat.cc;
   return 301 https://$server_name$request_uri;
}
server {
   listen 443 ssl;
   server_name datacat.cc www.datacat.cc;
   ssl_certificate /etc/letsencrypt/live/datacat.cc/fullchain.pem;
   ssl_certificate_key /etc/letsencrypt/live/datacat.cc/privkey.pem;
   ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
   ssl_prefer_server_ciphers on;
   ssl_ciphers AES256+EECDH:AES256+EDH:!aNULL;

   location / {
       proxy_pass http://18.190.57.240:3838;
       proxy_redirect http://18.190.57.240:3838/ https://$host/;
       proxy_http_version 1.1;
       proxy_set_header Upgrade $http_upgrade;
       proxy_set_header Connection $connection_upgrade;
       proxy_read_timeout 20d;
   }
}
```

# Enable the new block by creating a symlink

```
sudo ln -s /etc/nginx/sites-available/datacat.cc /etc/nginx/sites-enabled/datacat.cc
```

Disable the default block (?) since our server now handles all incoming traffic
```
sudo rm -f /etc/nginx/sites-enabled/default
```

Test the config:

sudo nginx -t

Restart nginx

sudo systemctl restart nginx


## hosting interactive R docs

sudo su - -c "R -e \"install.packages('rmarkdown', repos='http://cran.rstudio.com/')\""


Check that it worked at https://datacat.cc/sample-apps/rmd/

- Intall some additional software:
```
sudo apt-get -y install \
    nginx \
    gdebi-core \
    apache2-utils \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libxml2-dev \
    libxt-dev \
    libv8-dev
sudo apt-get update
```

- Install packages

```
sudo add-apt-repository ppa:marutter/c2d4u3.5
sudo apt update
sudo apt install r-cran-dplyr
sudo su - -c "R -e \"install.packages('devtools')\""
sudo su - -c "R -e \"install.packages('shinydashboard')\""
sudo su - -c "R -e \"install.packages('tidyverse')\""

sudo su - -c "R -e \"devtools::install_github('rstudio/DT')\""
sudo su - -c "R -e \"devtools::install_github('joebrew/vilaweb')\""
sudo su - -c "R -e \"devtools::install_github('databrew/databrew')\""
sudo su - -c "R -e \"install.packages('RCurl')\""
sudo su - -c "R -e \"install.packages('jsonlite')\""
sudo su - -c "R -e \"install.packages('rvest')\""
sudo su - -c "R -e \"install.packages('yaml')\""
sudo su - -c "R -e \"install.packages('rtweet')\""
sudo su - -c "R -e \"install.packages('ggmap')\""
sudo su - -c "R -e \"install.packages('ggthemes')\""
sudo su - -c "R -e \"install.packages('leaflet')\""
sudo su - -c "R -e \"install.packages('lubridate')\""
sudo su - -c "R -e \"install.packages('maps')\""
sudo su - -c "R -e \"install.packages('sp')\""
sudo su - -c "R -e \"install.packages('Hmisc')\""
sudo su - -c "R -e \"install.packages('extrafont')\""
sudo su - -c "R -e \"install.packages('grid')\""
sudo su - -c "R -e \"install.packages('scales')\""
sudo su - -c "R -e \"install.packages('DBI')\""
sudo su - -c "R -e \"install.packages('RPostgreSQL')\""
sudo su - -c "R -e \"install.packages('gsheet')\""
sudo su - -c "R -e \"install.packages('readr')\""
sudo su - -c "R -e \"install.packages('broom')\""
sudo su - -c "R -e \"install.packages('dbplyr')\""
sudo su - -c "R -e \"install.packages('haven')\""
sudo su - -c "R -e \"install.packages('jsonlite')\""
sudo su - -c "R -e \"install.packages('modelr')\""
sudo su - -c "R -e \"install.packages('tidyr')\""
sudo su - -c "R -e \"install.packages('dplyr')\""
sudo su - -c "R -e \"install.packages('tidyverse')\""

```

‘broom’, ‘dbplyr’, ‘haven’, ‘jsonlite’, ‘modelr’, ‘readr’, ‘tidyr’ are not available for package ‘tidyverse’






- Port from local to remote
```
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" ~/Documents/vilaweb/analyses/deleted_tweets ubuntu@datacat.cc:/home/ubuntu/Documents
```

- On remote machine, move to deploy area

```
sudo cp -r ~/Documents/deleted_tweets /srv/shiny-server/deleted_tweets
```

- Ensure permissions are okay and restart server:
```
sudo ufw allow 3838/tcp
sudo ufw allow 80/tcp
cd /srv/shiny-server
sudo chmod 555 deleted_tweets
sudo systemctl restart shiny-server
```
```
sudo systemctl restart shiny-server

```
