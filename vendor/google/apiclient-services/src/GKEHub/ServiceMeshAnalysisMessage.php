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

namespace Google\Service\GKEHub;

class ServiceMeshAnalysisMessage extends \Google\Collection
{
  protected $collection_key = 'resourcePaths';
  /**
   * @var array[]
   */
  public $args;
  /**
   * @var string
   */
  public $description;
  protected $messageBaseType = ServiceMeshAnalysisMessageBase::class;
  protected $messageBaseDataType = '';
  /**
   * @var string[]
   */
  public $resourcePaths;

  /**
   * @param array[]
   */
  public function setArgs($args)
  {
    $this->args = $args;
  }
  /**
   * @return array[]
   */
  public function getArgs()
  {
    return $this->args;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param ServiceMeshAnalysisMessageBase
   */
  public function setMessageBase(ServiceMeshAnalysisMessageBase $messageBase)
  {
    $this->messageBase = $messageBase;
  }
  /**
   * @return ServiceMeshAnalysisMessageBase
   */
  public function getMessageBase()
  {
    return $this->messageBase;
  }
  /**
   * @param string[]
   */
  public function setResourcePaths($resourcePaths)
  {
    $this->resourcePaths = $resourcePaths;
  }
  /**
   * @return string[]
   */
  public function getResourcePaths()
  {
    return $this->resourcePaths;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ServiceMeshAnalysisMessage::class, 'Google_Service_GKEHub_ServiceMeshAnalysisMessage');
