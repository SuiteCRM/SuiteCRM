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

namespace Google\Service\VMwareEngine;

class VmwareUpgradeComponent extends \Google\Model
{
  /**
   * @var string
   */
  public $componentType;
  /**
   * @var string
   */
  public $state;

  /**
   * @param string
   */
  public function setComponentType($componentType)
  {
    $this->componentType = $componentType;
  }
  /**
   * @return string
   */
  public function getComponentType()
  {
    return $this->componentType;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareUpgradeComponent::class, 'Google_Service_VMwareEngine_VmwareUpgradeComponent');
