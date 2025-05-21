<?php

namespace Robo\Config;

use Consolidation\Config\Util\ConfigOverlay;
use Consolidation\Config\ConfigInterface;

class Config extends ConfigOverlay implements GlobalOptionDefaultValuesInterface
{
    const PROGRESS_BAR_AUTO_DISPLAY_INTERVAL = 'options.progress-delay';
    const DEFAULT_PROGRESS_DELAY = 2;
    const SIMULATE = 'options.simulate';

    // Read-only configuration properties; changing these has no effect.
    const INTERACTIVE = 'options.interactive';
    const DECORATED = 'options.decorated';

    /**
     * @var array
     */
    protected $defaults;

    /**
     * Create a new configuration object, and initialize it with
     * the provided nested array containing configuration data.
     */
    public function __construct(array $data = null)
    {
        parent::__construct();

        $this->import($data ?: []);
        $this->defaults = $this->getGlobalOptionDefaultValues();
    }

    /**
     * {@inheritdoc}
     */
    public function import($data)
    {
        return $this->replace($data);
    }

    /**
     * {@inheritdoc}
     */
    public function replace($data)
    {
        $this->getContext(ConfigOverlay::DEFAULT_CONTEXT)->replace($data);
        return $this;
    }

    /**
     * {@inheritdoc}
     */
    public function combine($data)
    {
        $this->getContext(ConfigOverlay::DEFAULT_CONTEXT)->combine($data);
        return $this;
    }

    /**
     * Return an associative array containing all of the global configuration
     * options and their default values.
     *
     * @return array
     */
    public function getGlobalOptionDefaultValues()
    {
        $globalOptions =
        [
            self::PROGRESS_BAR_AUTO_DISPLAY_INTERVAL => self::DEFAULT_PROGRESS_DELAY,
            self::SIMULATE => false,
        ];
        return $this->trimPrefixFromGlobalOptions($globalOptions);
    }

    /**
     * Remove the 'options.' prefix from the global options list.
     *
     * @param array $globalOptions
     *
     * @return array
     */
    protected function trimPrefixFromGlobalOptions($globalOptions)
    {
        $result = [];
        foreach ($globalOptions as $option => $value) {
            $option = str_replace('options.', '', $option);
            $result[$option] = $value;
        }
        return $result;
    }

    /**
     * @deprecated Use $config->get(Config::SIMULATE)
     *
     * @return bool
     */
    public function isSimulated()
    {
        return $this->get(self::SIMULATE);
    }

    /**
     * @deprecated Use $config->set(Config::SIMULATE, true)
     *
     * @param bool $simulated
     *
     * @return $this
     */
    public function setSimulated($simulated = true)
    {
        return $this->set(self::SIMULATE, $simulated);
    }

    /**
     * @deprecated Use $config->get(Config::INTERACTIVE)
     *
     * @return bool
     */
    public function isInteractive()
    {
        return $this->get(self::INTERACTIVE);
    }

    /**
     * @deprecated Use $config->set(Config::INTERACTIVE, true)
     *
     * @param bool $interactive
     *
     * @return $this
     */
    public function setInteractive($interactive = true)
    {
        return $this->set(self::INTERACTIVE, $interactive);
    }

    /**
     * @deprecated Use $config->get(Config::DECORATED)
     *
     * @return bool
     */
    public function isDecorated()
    {
        return $this->get(self::DECORATED);
    }

    /**
     * @deprecated Use $config->set(Config::DECORATED, true)
     *
     * @param bool $decorated
     *
     * @return $this
     */
    public function setDecorated($decorated = true)
    {
        return $this->set(self::DECORATED, $decorated);
    }

    /**
     * @deprecated Use $config->set(Config::PROGRESS_BAR_AUTO_DISPLAY_INTERVAL, $interval)
     *
     * @param int $interval
     *
     * @return $this
     */
    public function setProgressBarAutoDisplayInterval($interval)
    {
        return $this->set(self::PROGRESS_BAR_AUTO_DISPLAY_INTERVAL, $interval);
    }
}
