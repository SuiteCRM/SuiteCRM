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

namespace Google\Service\APIManagement;

class ApiObservation extends \Google\Collection
{
  protected $collection_key = 'tags';
  /**
   * @var string
   */
  public $apiOperationCount;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $hostname;
  /**
   * @var string
   */
  public $lastEventDetectedTime;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $serverIps;
  /**
   * @var string[]
   */
  public $sourceLocations;
  /**
   * @var string
   */
  public $style;
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setApiOperationCount($apiOperationCount)
  {
    $this->apiOperationCount = $apiOperationCount;
  }
  /**
   * @return string
   */
  public function getApiOperationCount()
  {
    return $this->apiOperationCount;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setHostname($hostname)
  {
    $this->hostname = $hostname;
  }
  /**
   * @return string
   */
  public function getHostname()
  {
    return $this->hostname;
  }
  /**
   * @param string
   */
  public function setLastEventDetectedTime($lastEventDetectedTime)
  {
    $this->lastEventDetectedTime = $lastEventDetectedTime;
  }
  /**
   * @return string
   */
  public function getLastEventDetectedTime()
  {
    return $this->lastEventDetectedTime;
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
   * @param string[]
   */
  public function setServerIps($serverIps)
  {
    $this->serverIps = $serverIps;
  }
  /**
   * @return string[]
   */
  public function getServerIps()
  {
    return $this->serverIps;
  }
  /**
   * @param string[]
   */
  public function setSourceLocations($sourceLocations)
  {
    $this->sourceLocations = $sourceLocations;
  }
  /**
   * @return string[]
   */
  public function getSourceLocations()
  {
    return $this->sourceLocations;
  }
  /**
   * @param string
   */
  public function setStyle($style)
  {
    $this->style = $style;
  }
  /**
   * @return string
   */
  public function getStyle()
  {
    return $this->style;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ApiObservation::class, 'Google_Service_APIManagement_ApiObservation');
