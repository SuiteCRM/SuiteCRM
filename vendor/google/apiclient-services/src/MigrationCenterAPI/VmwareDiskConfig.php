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

class VmwareDiskConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $backingType;
  /**
   * @var string
   */
  public $rdmCompatibility;
  /**
   * @var bool
   */
  public $shared;
  /**
   * @var string
   */
  public $vmdkMode;

  /**
   * @param string
   */
  public function setBackingType($backingType)
  {
    $this->backingType = $backingType;
  }
  /**
   * @return string
   */
  public function getBackingType()
  {
    return $this->backingType;
  }
  /**
   * @param string
   */
  public function setRdmCompatibility($rdmCompatibility)
  {
    $this->rdmCompatibility = $rdmCompatibility;
  }
  /**
   * @return string
   */
  public function getRdmCompatibility()
  {
    return $this->rdmCompatibility;
  }
  /**
   * @param bool
   */
  public function setShared($shared)
  {
    $this->shared = $shared;
  }
  /**
   * @return bool
   */
  public function getShared()
  {
    return $this->shared;
  }
  /**
   * @param string
   */
  public function setVmdkMode($vmdkMode)
  {
    $this->vmdkMode = $vmdkMode;
  }
  /**
   * @return string
   */
  public function getVmdkMode()
  {
    return $this->vmdkMode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareDiskConfig::class, 'Google_Service_MigrationCenterAPI_VmwareDiskConfig');
