<?php

require_once __DIR__ . '/../../../../../include/GoogleSync/GoogleSync.php';

/**
 * GoogleSyncMock
 *
 * @author gyula
 */
#[\AllowDynamicProperties]
class GoogleSyncMock extends GoogleSync
{
    public function setProperty($key, $value): void
    {
        $this->$key = $value;
    }

    public function getProperty($key)
    {
        if (!isset($this->$key)) {
            if (!isset(parent::$key)) {
                throw new Exception('Key is not set: ' . $key);
            }
            return parent::$key;
        }
        return $this->$key;
    }

    public function callMethod($name, $params = [])
    {
        if (!method_exists($this, $name)) {
            throw new Exception('Method is not exists: ' . $name);
        }

        return call_user_func_array([$this, $name], (array)$params);
    }
}
