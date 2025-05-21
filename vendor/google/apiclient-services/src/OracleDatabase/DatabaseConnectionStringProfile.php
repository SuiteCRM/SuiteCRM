<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\OracleDatabase;

class DatabaseConnectionStringProfile extends \Google\Model
{
  /**
   * @var string
   */
  public $consumerGroup;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $hostFormat;
  /**
   * @var bool
   */
  public $isRegional;
  /**
   * @var string
   */
  public $protocol;
  /**
   * @var string
   */
  public $sessionMode;
  /**
   * @var string
   */
  public $syntaxFormat;
  /**
   * @var string
   */
  public $tlsAuthentication;
  /**
   * @var string
   */
  public $value;

  /**
   * @param string
   */
  public function setConsumerGroup($consumerGroup)
  {
    $this->consumerGroup = $consumerGroup;
  }
  /**
   * @return string
   */
  public function getConsumerGroup()
  {
    return $this->consumerGroup;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setHostFormat($hostFormat)
  {
    $this->hostFormat = $hostFormat;
  }
  /**
   * @return string
   */
  public function getHostFormat()
  {
    return $this->hostFormat;
  }
  /**
   * @param bool
   */
  public function setIsRegional($isRegional)
  {
    $this->isRegional = $isRegional;
  }
  /**
   * @return bool
   */
  public function getIsRegional()
  {
    return $this->isRegional;
  }
  /**
   * @param string
   */
  public function setProtocol($protocol)
  {
    $this->protocol = $protocol;
  }
  /**
   * @return string
   */
  public function getProtocol()
  {
    return $this->protocol;
  }
  /**
   * @param string
   */
  public function setSessionMode($sessionMode)
  {
    $this->sessionMode = $sessionMode;
  }
  /**
   * @return string
   */
  public function getSessionMode()
  {
    return $this->sessionMode;
  }
  /**
   * @param string
   */
  public function setSyntaxFormat($syntaxFormat)
  {
    $this->syntaxFormat = $syntaxFormat;
  }
  /**
   * @return string
   */
  public function getSyntaxFormat()
  {
    return $this->syntaxFormat;
  }
  /**
   * @param string
   */
  public function setTlsAuthentication($tlsAuthentication)
  {
    $this->tlsAuthentication = $tlsAuthentication;
  }
  /**
   * @return string
   */
  public function getTlsAuthentication()
  {
    return $this->tlsAuthentication;
  }
  /**
   * @param string
   */
  public function setValue($value)
  {
    $this->value = $value;
  }
  /**
   * @return string
   */
  public function getValue()
  {
    return $this->value;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DatabaseConnectionStringProfile::class, 'Google_Service_OracleDatabase_DatabaseConnectionStringProfile');
