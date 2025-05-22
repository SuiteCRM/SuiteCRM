<?php

/*
 * Copyright (c) 2016 - 2020 Itspire.
 * This software is licensed under the BSD-3-Clause license. (see LICENSE.md for full license)
 * All Right Reserved.
 */

declare(strict_types=1);

namespace Itspire\MonologLoki\Handler;

use Itspire\MonologLoki\Formatter\LokiFormatter;
use Monolog\Formatter\FormatterInterface;
use Monolog\Handler\AbstractProcessingHandler;
use Monolog\Handler\Curl;
use Monolog\Logger;

class LokiHandler extends AbstractProcessingHandler
{
    /** the scheme, hostname and port to the Loki system */
    protected ?string $entrypoint;

    /** the identifiers for Basic Authentication to the Loki system */
    protected array $basicAuth = [];

    /** the name of the system (hostname) sending log messages to Loki */
    protected ?string $systemName;

    /** the list of default context variables to be sent to the Loki system */
    protected array $globalContext = [];

    /** the list of default labels to be sent to the Loki system */
    protected array $globalLabels = [];

    /** custom curl options */
    protected array $customCurlOptions = [];

    /** curl options which cannot be customized */
    protected array $nonCustomizableCurlOptions = [
        CURLOPT_CUSTOMREQUEST,
        CURLOPT_RETURNTRANSFER,
        CURLOPT_POSTFIELDS,
        CURLOPT_HTTPHEADER,
    ];

    /** @return false|null|resource */
    private $connection;

    public function __construct(array $apiConfig, $level = Logger::DEBUG, $bubble = true)
    {
        if (!function_exists('json_encode')) {
            throw new \RuntimeException('PHP\'s json extension is required to use Monolog\'s LokiHandler');
        }
        parent::__construct($level, $bubble);
        $this->entrypoint = $this->getEntrypoint($apiConfig['entrypoint']);
        $this->globalContext = $apiConfig['context'] ?? [];
        $this->globalLabels = $apiConfig['labels'] ?? [];
        $this->systemName = $apiConfig['client_name'] ?? null;
        $this->customCurlOptions = $this->determineValidCustomCurlOptions($apiConfig['curl_options'] ?? []);
        if (isset($apiConfig['auth']['basic'])) {
            $this->basicAuth = (2 === count($apiConfig['auth']['basic'])) ? $apiConfig['auth']['basic'] : [];
        }
    }

    private function determineValidCustomCurlOptions(array $configuredCurlOptions): array
    {
        foreach ($this->nonCustomizableCurlOptions as $option) {
            unset($configuredCurlOptions[$option]);
        }

        return $configuredCurlOptions;
    }

    private function getEntrypoint(string $entrypoint): string
    {
        if ('/' !== substr($entrypoint, -1)) {
            return $entrypoint;
        }

        return substr($entrypoint, 0, -1);
    }

    /** @throws \JsonException */
    public function handleBatch(array $records): void
    {
        $rows = [];
        foreach ($records as $record) {
            if (!$this->isHandling($record)) {
                continue;
            }

            $record = $this->processRecord($record);
            $rows[] = $this->getFormatter()->format($record);
        }

        $this->sendPacket(['streams' => $rows]);
    }

    /** @throws \JsonException */
    private function sendPacket(array $packet): void
    {
        $payload = json_encode($packet, JSON_THROW_ON_ERROR | JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_SLASHES);
        $url = sprintf('%s/loki/api/v1/push', $this->entrypoint);
        if (null === $this->connection) {
            $this->connection = curl_init($url);

            if (!$this->connection) {
                throw new \LogicException('Unable to connect to ' . $url);
            }
        }

        if (false !== $this->connection) {
            $curlOptions = array_replace(
                [
                    CURLOPT_CONNECTTIMEOUT_MS => 100,
                    CURLOPT_TIMEOUT_MS => 200,
                    CURLOPT_CUSTOMREQUEST => 'POST',
                    CURLOPT_RETURNTRANSFER => true,
                    CURLOPT_POSTFIELDS => $payload,
                    CURLOPT_HTTPHEADER => [
                        'Content-Type: application/json',
                        'Content-Length: ' . strlen($payload),
                    ],
                ],
                $this->customCurlOptions
            );

            if (!empty($this->basicAuth)) {
                $curlOptions[CURLOPT_HTTPAUTH] = CURLAUTH_BASIC;
                $curlOptions[CURLOPT_USERPWD] = implode(':', $this->basicAuth);
            }

            curl_setopt_array($this->connection, $curlOptions);

            // Should Loki not be available yet,
            // too many retries attempts cause some processes to hang,
            // awaiting retries results so we limit to one attempt.
            // Note :  Loki is a network related logging system ! It should not be the only logging system relied on.
            Curl\Util::execute($this->connection, 1, false);
        }
    }

    protected function getDefaultFormatter(): FormatterInterface
    {
        return new LokiFormatter($this->globalLabels, $this->globalContext, $this->systemName);
    }

    /** @throws \JsonException */
    protected function write(array $record): void
    {
        $this->sendPacket(['streams' => [$record['formatted']]]);
    }
}
