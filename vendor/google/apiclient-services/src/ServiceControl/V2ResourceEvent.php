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

namespace Google\Service\ServiceControl;

class V2ResourceEvent extends \Google\Model
{
  /**
   * @var string
   */
  public $contextId;
  /**
   * @var string
   */
  public $destinations;
  protected $parentType = ServicecontrolResource::class;
  protected $parentDataType = '';
  /**
   * @var string
   */
  public $path;
  /**
   * @var array[]
   */
  public $payload;
  protected $resourceType = ServicecontrolResource::class;
  protected $resourceDataType = '';
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setContextId($contextId)
  {
    $this->contextId = $contextId;
  }
  /**
   * @return string
   */
  public function getContextId()
  {
    return $this->contextId;
  }
  /**
   * @param string
   */
  public function setDestinations($destinations)
  {
    $this->destinations = $destinations;
  }
  /**
   * @return string
   */
  public function getDestinations()
  {
    return $this->destinations;
  }
  /**
   * @param ServicecontrolResource
   */
  public function setParent(ServicecontrolResource $parent)
  {
    $this->parent = $parent;
  }
  /**
   * @return ServicecontrolResource
   */
  public function getParent()
  {
    return $this->parent;
  }
  /**
   * @param string
   */
  public function setPath($path)
  {
    $this->path = $path;
  }
  /**
   * @return string
   */
  public function getPath()
  {
    return $this->path;
  }
  /**
   * @param array[]
   */
  public function setPayload($payload)
  {
    $this->payload = $payload;
  }
  /**
   * @return array[]
   */
  public function getPayload()
  {
    return $this->payload;
  }
  /**
   * @param ServicecontrolResource
   */
  public function setResource(ServicecontrolResource $resource)
  {
    $this->resource = $resource;
  }
  /**
   * @return ServicecontrolResource
   */
  public function getResource()
  {
    return $this->resource;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(V2ResourceEvent::class, 'Google_Service_ServiceControl_V2ResourceEvent');
