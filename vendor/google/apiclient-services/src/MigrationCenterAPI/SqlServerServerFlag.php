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

namespace Google\Service\MigrationCenterAPI;

class SqlServerServerFlag extends \Google\Model
{
  /**
   * @var string
   */
  public $serverFlagName;
  /**
   * @var string
   */
  public $value;
  /**
   * @var string
   */
  public $valueInUse;

  /**
   * @param string
   */
  public function setServerFlagName($serverFlagName)
  {
    $this->serverFlagName = $serverFlagName;
  }
  /**
   * @return string
   */
  public function getServerFlagName()
  {
    return $this->serverFlagName;
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
  /**
   * @param string
   */
  public function setValueInUse($valueInUse)
  {
    $this->valueInUse = $valueInUse;
  }
  /**
   * @return string
   */
  public function getValueInUse()
  {
    return $this->valueInUse;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SqlServerServerFlag::class, 'Google_Service_MigrationCenterAPI_SqlServerServerFlag');
