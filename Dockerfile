FROM php:8.2-apache

# Install system dependencies and PHP extensions SuiteCRM requires
RUN set -eux; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        libzip-dev \
        libpng-dev \
        libjpeg-dev \
        libfreetype6-dev \
        libc-client-dev \
        libkrb5-dev \
        libldap2-dev \
        libxml2-dev \
        libonig-dev \
        cron \
        unzip \
    ; \
    docker-php-ext-configure gd --with-freetype --with-jpeg; \
    docker-php-ext-configure imap --with-kerberos --with-imap-ssl; \
    docker-php-ext-install -j$(nproc) \
        mysqli \
        pdo_mysql \
        gd \
        zip \
        imap \
        mbstring \
        intl \
        curl \
        opcache \
        ldap \
        xml \
    ; \
    a2enmod rewrite; \
    apt-get clean; \
    rm -rf /var/lib/apt/lists/*

# PHP configuration
RUN { \
        echo 'memory_limit = 256M'; \
        echo 'upload_max_filesize = 64M'; \
        echo 'post_max_size = 64M'; \
        echo 'max_execution_time = 300'; \
        echo 'max_input_time = 300'; \
        echo 'display_errors = Off'; \
        echo 'log_errors = On'; \
        echo 'error_log = /dev/stderr'; \
        echo 'date.timezone = UTC'; \
    } > /usr/local/etc/php/conf.d/suitecrm.ini

# Apache virtual host
COPY docker/apache.conf /etc/apache2/sites-available/000-default.conf

# Install Composer
COPY --from=composer:2 /usr/bin/composer /usr/bin/composer

# Copy SuiteCRM source
COPY . /var/www/html/

# Set up the application directory
WORKDIR /var/www/html

# Install Composer dependencies
RUN composer install --no-interaction --prefer-dist --no-dev --optimize-autoloader

# Create required directories and set permissions
RUN mkdir -p cache custom modules upload && \
    chown -R www-data:www-data /var/www/html && \
    chmod -R 755 /var/www/html && \
    chmod -R 775 cache custom modules upload

# Copy entrypoint
COPY docker/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["apache2-foreground"]
