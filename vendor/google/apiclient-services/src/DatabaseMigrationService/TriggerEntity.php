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

namespace Google\Service\DatabaseMigrationService;

class TriggerEntity extends \Google\Collection
{
  protected $collection_key = 'triggeringEvents';
  /**
   * @var array[]
   */
  public $customFeatures;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $sqlCode;
  /**
   * @var string
   */
  public $triggerType;
  /**
   * @var string[]
   */
  public $triggeringEvents;

  /**
   * @param array[]
   */
  public function setCustomFeatures($customFeatures)
  {
    $this->customFeatures = $customFeatures;
  }
  /**
   * @return array[]
   */
  public function getCustomFeatures()
  {
    return $this->customFeatures;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setSqlCode($sqlCode)
  {
    $this->sqlCode = $sqlCode;
  }
  /**
   * @return string
   */
  public function getSqlCode()
  {
    return $this->sqlCode;
  }
  /**
   * @param string
   */
  public function setTriggerType($triggerType)
  {
    $this->triggerType = $triggerType;
  }
  /**
   * @return string
   */
  public function getTriggerType()
  {
    return $this->triggerType;
  }
  /**
   * @param string[]
   */
  public function setTriggeringEvents($triggeringEvents)
  {
    $this->triggeringEvents = $triggeringEvents;
  }
  /**
   * @return string[]
   */
  public function getTriggeringEvents()
  {
    return $this->triggeringEvents;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TriggerEntity::class, 'Google_Service_DatabaseMigrationService_TriggerEntity');
