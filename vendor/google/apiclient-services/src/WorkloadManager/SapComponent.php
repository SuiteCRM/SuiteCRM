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

class SapComponent extends \Google\Collection
{
  protected $collection_key = 'resources';
  /**
   * @var string[]
   */
  public $haHosts;
  protected $resourcesType = CloudResource::class;
  protected $resourcesDataType = 'array';
  /**
   * @var string
   */
  public $sid;
  /**
   * @var string
   */
  public $topologyType;

  /**
   * @param string[]
   */
  public function setHaHosts($haHosts)
  {
    $this->haHosts = $haHosts;
  }
  /**
   * @return string[]
   */
  public function getHaHosts()
  {
    return $this->haHosts;
  }
  /**
   * @param CloudResource[]
   */
  public function setResources($resources)
  {
    $this->resources = $resources;
  }
  /**
   * @return CloudResource[]
   */
  public function getResources()
  {
    return $this->resources;
  }
  /**
   * @param string
   */
  public function setSid($sid)
  {
    $this->sid = $sid;
  }
  /**
   * @return string
   */
  public function getSid()
  {
    return $this->sid;
  }
  /**
   * @param string
   */
  public function setTopologyType($topologyType)
  {
    $this->topologyType = $topologyType;
  }
  /**
   * @return string
   */
  public function getTopologyType()
  {
    return $this->topologyType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SapComponent::class, 'Google_Service_WorkloadManager_SapComponent');
