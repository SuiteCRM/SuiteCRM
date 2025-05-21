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

namespace Google\Service\WorkloadManager;

class SapDiscoveryComponentReplicationSite extends \Google\Model
{
  protected $componentType = SapDiscoveryComponent::class;
  protected $componentDataType = '';
  /**
   * @var string
   */
  public $sourceSite;

  /**
   * @param SapDiscoveryComponent
   */
  public function setComponent(SapDiscoveryComponent $component)
  {
    $this->component = $component;
  }
  /**
   * @return SapDiscoveryComponent
   */
  public function getComponent()
  {
    return $this->component;
  }
  /**
   * @param string
   */
  public function setSourceSite($sourceSite)
  {
    $this->sourceSite = $sourceSite;
  }
  /**
   * @return string
   */
  public function getSourceSite()
  {
    return $this->sourceSite;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SapDiscoveryComponentReplicationSite::class, 'Google_Service_WorkloadManager_SapDiscoveryComponentReplicationSite');
