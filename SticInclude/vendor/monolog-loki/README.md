# Itspire - Monolog Loki

This library follows the PSR-12 convention.

# Usage

## Recommended Usage

Since Loki log handling uses a remote server, logging is prone to be subject of timeout, network shortage and so on.
To avoid your application being broken in such a case, we recommend wrapping the handler in a WhatFailureGroupHandler

## Native
```php
use Itspire\MonologLoki\Handler\LokiHandler;
use Monolog\Handler\WhatFailureGroupHandler;

$handler = new WhatFailureGroupHandler(
    [
        new LokiHandler(
            [
                'entrypoint' => 'https://loki:3100',
                'context' => [
                    // Set here your globally applicable context variables
                ],
                'labels' => [
                    // Set here your globally applicable labels
                ],
                'client_name' => 'your_host_name', // Here set a unique identifier for the client host
                // Optional : if you're using basic auth to authentify
                'auth' => [
                    'basic' => ['user', 'password'],
                ],
                // Optional : Override the default curl options with custom values
                'curl_options' => [
                    CURLOPT_CONNECTTIMEOUT_MS => 500,
                    CURLOPT_TIMEOUT_MS => 600
                ]
            ]
        )
    ]
);
```

### Non-customizable curl options
The following options are not customizable in the configuration:

- `CURLOPT_CUSTOMREQUEST`
- `CURLOPT_RETURNTRANSFER`
- `CURLOPT_POSTFIELDS`
- `CURLOPT_HTTPHEADER`

## Symfony App

### Configure LokiHandler Service
```yaml
  Itspire\MonologLoki\Handler\LokiHandler:
    arguments:
      $apiConfig:
        entrypoint: 'http://loki:3100'
        context:
          app: My-app
        labels:
          env: '%env(APP_ENV)%'
        client_name: my_app_server
        auth:
          basic:
            user: username
            password: password
        curl_options:
          !php/const CURLOPT_CONNECTTIMEOUT_MS: 500,
          !php/const CURLOPT_TIMEOUT_MS: 600
```
Note : 
We're currently working on a possible bundle based implementation for Symfony but at the moment, this is the way.


### Configure Monolog to use Loki Handler
```yaml
monolog:
  handlers:
    loki:
      type: service
      id: Itspire\MonologLoki\Handler\LokiHandler

    my_loki_handler:
      type:   whatfailuregroup
      members: [loki]
      level: debug
      process_psr_3_messages: true # optional but we find it rather useful (Note : native handler required to use)
```

## Laravel App

### Add Loki to config/logging.php
```php
'loki' => [
    'driver'         => 'monolog',
    'level'          => env('LOG_LEVEL', 'debug'),
    'handler'        => \Itspire\MonologLoki\Handler\LokiHandler::class,
    'formatter'      => \Itspire\MonologLoki\Formatter\LokiFormatter::class,
    'formatter_with' => [
        'labels' => [],
        'context' => [],
        'systemName' => env('LOKI_SYSTEM_NAME', null),
        'extraPrefix' => env('LOKI_EXTRA_PREFIX', ''),
        'contextPrefix' => env('LOKI_CONTEXT_PREFIX', '')
    ],
    'handler_with'   => [
        'apiConfig'  => [
            'entrypoint'  => env('LOKI_ENTRYPOINT', "http://localhost:3100"),
            'context'     => [],
            'labels'      => [],
            'client_name' => '',
            'auth' => [
                'basic' => [
                    env('LOKI_AUTH_BASIC_USER', ''), 
                    env('LOKI_AUTH_BASIC_PASSWORD', '')
                ],
            ],
        ],
    ],
],
```

### Set env vars
```
LOKI_ENTRYPOINT="http://loki:3100"
LOKI_AUTH_BASIC_USER=
LOKI_AUTH_BASIC_PASSWORD=
LOKI_SYSTEM_NAME=null
LOKI_CONTEXT_PREFIX="context_"
LOKI_EXTRA_PREFIX=
```
These vars can be injected by Kubernetes, Docker or simply by setting them on the .env file

### Laravel with WhatFailureGroupHandler
Since Loki log handling uses a remote server, logging is prone to be subject of timeout, network shortage and so on. To avoid your application being broken in such a case, we recommend wrapping the handler in a WhatFailureGroupHandler. 

Create a custom Log handler and wrap the `LokiHandler` with a `WhatFailureGroupHandler`.
```php
namespace App\Logging;

use Itspire\MonologLoki\Formatter\LokiFormatter;
use Itspire\MonologLoki\Handler\LokiHandler;
use Monolog\Handler\WhatFailureGroupHandler;
use Monolog\Logger;

class LokiNoFailureHandler
{
    public function __invoke(array $config)
    {
        return new Logger('loki-no-failure', [
            new WhatFailureGroupHandler([
                (new LokiHandler($config['handler_with']['apiConfig'], $config['level']))
                    ->setFormatter(new LokiFormatter(...array_values($config['formatter_with'])))
            ])
        ]);
    }
}

```
Update the config accordingly: 

```php
'loki' => [
    'driver'    => 'custom',
    'level'     => env('LOG_LEVEL', 'debug'),
    'via'       => \App\Logging\LokiNoFailureHandler::class,
    'formatter_with' => [
        'labels' => env('LOKI_LABELS', ''),
        'context' => [],
        'systemName' => env('LOKI_SYSTEM_NAME', ''),
        'extraPrefix' => env('LOKI_EXTRA_PREFIX', ''),
        'contextPrefix' => env('LOKI_CONTEXT_PREFIX', '')
    ],
    'handler_with'   => [
        'apiConfig'  => [
            'entrypoint'  => env('LOKI_ENTRYPOINT', "http://localhost:3100"),
            'context'     => [],
            'labels'      => [],
            'client_name' => '',
            'auth' => [
                'basic' => [
                    env('LOKI_AUTH_BASIC_USER', ''),
                    env('LOKI_AUTH_BASIC_PASSWORD', '')
                ],
            ]
        ],
    ],
],
```

# Testing
In order to test using the provided docker-compose file, you'll need an up-to-date docker/docker-compose installation
You can start the Loki container by navigating to src/main/test/docker and running 
```shell script
docker-compose up
```

If you're testing from a local php installation, you'll need to retrieve the Loki container ip with :
```shell script
docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' itspire-monolog-loki_loki_1
```

If you're testing from containerized php, you'll need to start the container with an extra host named Loki 
mapped to your current host ip, using the following option :
```shell script
--add-host loki:{your_host_ip}
```

Run the test using phpunit and you can verify that posting to Loki works
by running the following from your host terminal : 
```shell script
curl -G -s  "http://localhost:3100/loki/api/v1/query" --data-urlencode 'query={channel="test"}' | jq
```

For each time you ran the tests, you should see a log entry looking like the following : 
```json
{
    "stream": {
        "channel": "test",
        "host": "f2bbe48b0204",
        "level_name": "WARNING"
    },
    "values": [
        [
        "1591627127000000000",
        "{\"message\":\"test\",\"level\":300,\"level_name\":\"WARNING\",\"channel\":\"test\",\"datetime\":\"2020-06-08 14:38:47\",\"ctxt_data\":\"[object] (stdClass: {})\",\"ctxt_foo\":\"34\"}"
        ]
    ]
}
```
