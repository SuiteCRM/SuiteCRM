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

namespace Google\Service\SecurityCommandCenter;

class Simulation extends \Google\Collection
{
  protected $collection_key = 'resourceValueConfigsMetadata';
  /**
   * @var string
   */
  public $cloudProvider;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $name;
  protected $resourceValueConfigsMetadataType = ResourceValueConfigMetadata::class;
  protected $resourceValueConfigsMetadataDataType = 'array';

  /**
   * @param string
   */
  public function setCloudProvider($cloudProvider)
  {
    $this->cloudProvider = $cloudProvider;
  }
  /**
   * @return string
   */
  public function getCloudProvider()
  {
    return $this->cloudProvider;
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
   * @param ResourceValueConfigMetadata[]
   */
  public function setResourceValueConfigsMetadata($resourceValueConfigsMetadata)
  {
    $this->resourceValueConfigsMetadata = $resourceValueConfigsMetadata;
  }
  /**
   * @return ResourceValueConfigMetadata[]
   */
  public function getResourceValueConfigsMetadata()
  {
    return $this->resourceValueConfigsMetadata;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Simulation::class, 'Google_Service_SecurityCommandCenter_Simulation');
