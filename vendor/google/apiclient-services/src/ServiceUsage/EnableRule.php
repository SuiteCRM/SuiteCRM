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

namespace Google\Service\ServiceUsage;

class EnableRule extends \Google\Collection
{
  protected $collection_key = 'values';
  /**
   * @var string
   */
  public $enableType;
  /**
   * @var string[]
   */
  public $groups;
  /**
   * @var string[]
   */
  public $services;
  /**
   * @var string[]
   */
  public $values;

  /**
   * @param string
   */
  public function setEnableType($enableType)
  {
    $this->enableType = $enableType;
  }
  /**
   * @return string
   */
  public function getEnableType()
  {
    return $this->enableType;
  }
  /**
   * @param string[]
   */
  public function setGroups($groups)
  {
    $this->groups = $groups;
  }
  /**
   * @return string[]
   */
  public function getGroups()
  {
    return $this->groups;
  }
  /**
   * @param string[]
   */
  public function setServices($services)
  {
    $this->services = $services;
  }
  /**
   * @return string[]
   */
  public function getServices()
  {
    return $this->services;
  }
  /**
   * @param string[]
   */
  public function setValues($values)
  {
    $this->values = $values;
  }
  /**
   * @return string[]
   */
  public function getValues()
  {
    return $this->values;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnableRule::class, 'Google_Service_ServiceUsage_EnableRule');
