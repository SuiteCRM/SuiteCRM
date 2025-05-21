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

namespace Google\Service\GKEOnPrem;

class BareMetalKubeletConfig extends \Google\Model
{
  /**
   * @var int
   */
  public $registryBurst;
  /**
   * @var int
   */
  public $registryPullQps;
  /**
   * @var bool
   */
  public $serializeImagePullsDisabled;

  /**
   * @param int
   */
  public function setRegistryBurst($registryBurst)
  {
    $this->registryBurst = $registryBurst;
  }
  /**
   * @return int
   */
  public function getRegistryBurst()
  {
    return $this->registryBurst;
  }
  /**
   * @param int
   */
  public function setRegistryPullQps($registryPullQps)
  {
    $this->registryPullQps = $registryPullQps;
  }
  /**
   * @return int
   */
  public function getRegistryPullQps()
  {
    return $this->registryPullQps;
  }
  /**
   * @param bool
   */
  public function setSerializeImagePullsDisabled($serializeImagePullsDisabled)
  {
    $this->serializeImagePullsDisabled = $serializeImagePullsDisabled;
  }
  /**
   * @return bool
   */
  public function getSerializeImagePullsDisabled()
  {
    return $this->serializeImagePullsDisabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(BareMetalKubeletConfig::class, 'Google_Service_GKEOnPrem_BareMetalKubeletConfig');
