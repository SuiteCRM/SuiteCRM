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

namespace Google\Service\VMMigrationService;

class VmCapabilities extends \Google\Collection
{
  protected $collection_key = 'osCapabilities';
  /**
   * @var string
   */
  public $lastOsCapabilitiesUpdateTime;
  /**
   * @var string[]
   */
  public $osCapabilities;

  /**
   * @param string
   */
  public function setLastOsCapabilitiesUpdateTime($lastOsCapabilitiesUpdateTime)
  {
    $this->lastOsCapabilitiesUpdateTime = $lastOsCapabilitiesUpdateTime;
  }
  /**
   * @return string
   */
  public function getLastOsCapabilitiesUpdateTime()
  {
    return $this->lastOsCapabilitiesUpdateTime;
  }
  /**
   * @param string[]
   */
  public function setOsCapabilities($osCapabilities)
  {
    $this->osCapabilities = $osCapabilities;
  }
  /**
   * @return string[]
   */
  public function getOsCapabilities()
  {
    return $this->osCapabilities;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmCapabilities::class, 'Google_Service_VMMigrationService_VmCapabilities');
