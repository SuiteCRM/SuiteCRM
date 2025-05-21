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

namespace Google\Service\NetworkSecurity;

class MirroringEndpointGroupAssociation extends \Google\Collection
{
  protected $collection_key = 'locationsDetails';
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string[]
   */
  public $labels;
  protected $locationsDetailsType = MirroringEndpointGroupAssociationLocationDetails::class;
  protected $locationsDetailsDataType = 'array';
  /**
   * @var string
   */
  public $mirroringEndpointGroup;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  /**
   * @var bool
   */
  public $reconciling;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;

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
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param MirroringEndpointGroupAssociationLocationDetails[]
   */
  public function setLocationsDetails($locationsDetails)
  {
    $this->locationsDetails = $locationsDetails;
  }
  /**
   * @return MirroringEndpointGroupAssociationLocationDetails[]
   */
  public function getLocationsDetails()
  {
    return $this->locationsDetails;
  }
  /**
   * @param string
   */
  public function setMirroringEndpointGroup($mirroringEndpointGroup)
  {
    $this->mirroringEndpointGroup = $mirroringEndpointGroup;
  }
  /**
   * @return string
   */
  public function getMirroringEndpointGroup()
  {
    return $this->mirroringEndpointGroup;
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
   * @param string
   */
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param bool
   */
  public function setReconciling($reconciling)
  {
    $this->reconciling = $reconciling;
  }
  /**
   * @return bool
   */
  public function getReconciling()
  {
    return $this->reconciling;
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
class_alias(MirroringEndpointGroupAssociation::class, 'Google_Service_NetworkSecurity_MirroringEndpointGroupAssociation');
