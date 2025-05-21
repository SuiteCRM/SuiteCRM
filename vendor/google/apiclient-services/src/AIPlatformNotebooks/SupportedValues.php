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

namespace Google\Service\AIPlatformNotebooks;

class SupportedValues extends \Google\Collection
{
  protected $collection_key = 'machineTypes';
  /**
   * @var string[]
   */
  public $acceleratorTypes;
  /**
   * @var string[]
   */
  public $machineTypes;

  /**
   * @param string[]
   */
  public function setAcceleratorTypes($acceleratorTypes)
  {
    $this->acceleratorTypes = $acceleratorTypes;
  }
  /**
   * @return string[]
   */
  public function getAcceleratorTypes()
  {
    return $this->acceleratorTypes;
  }
  /**
   * @param string[]
   */
  public function setMachineTypes($machineTypes)
  {
    $this->machineTypes = $machineTypes;
  }
  /**
   * @return string[]
   */
  public function getMachineTypes()
  {
    return $this->machineTypes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SupportedValues::class, 'Google_Service_AIPlatformNotebooks_SupportedValues');
