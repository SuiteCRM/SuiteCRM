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

namespace Google\Service\Compute;

class InterconnectRemoteLocation extends \Google\Collection
{
  protected $collection_key = 'permittedConnections';
  /**
   * @var string
   */
  public $address;
  protected $attachmentConfigurationConstraintsType = InterconnectAttachmentConfigurationConstraints::class;
  protected $attachmentConfigurationConstraintsDataType = '';
  /**
   * @var string
   */
  public $city;
  protected $constraintsType = InterconnectRemoteLocationConstraints::class;
  protected $constraintsDataType = '';
  /**
   * @var string
   */
  public $continent;
  /**
   * @var string
   */
  public $creationTimestamp;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $facilityProvider;
  /**
   * @var string
   */
  public $facilityProviderFacilityId;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $lacp;
  /**
   * @var int
   */
  public $maxLagSize100Gbps;
  /**
   * @var int
   */
  public $maxLagSize10Gbps;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $peeringdbFacilityId;
  protected $permittedConnectionsType = InterconnectRemoteLocationPermittedConnections::class;
  protected $permittedConnectionsDataType = 'array';
  /**
   * @var string
   */
  public $remoteService;
  /**
   * @var string
   */
  public $selfLink;
  /**
   * @var string
   */
  public $status;

  /**
   * @param string
   */
  public function setAddress($address)
  {
    $this->address = $address;
  }
  /**
   * @return string
   */
  public function getAddress()
  {
    return $this->address;
  }
  /**
   * @param InterconnectAttachmentConfigurationConstraints
   */
  public function setAttachmentConfigurationConstraints(InterconnectAttachmentConfigurationConstraints $attachmentConfigurationConstraints)
  {
    $this->attachmentConfigurationConstraints = $attachmentConfigurationConstraints;
  }
  /**
   * @return InterconnectAttachmentConfigurationConstraints
   */
  public function getAttachmentConfigurationConstraints()
  {
    return $this->attachmentConfigurationConstraints;
  }
  /**
   * @param string
   */
  public function setCity($city)
  {
    $this->city = $city;
  }
  /**
   * @return string
   */
  public function getCity()
  {
    return $this->city;
  }
  /**
   * @param InterconnectRemoteLocationConstraints
   */
  public function setConstraints(InterconnectRemoteLocationConstraints $constraints)
  {
    $this->constraints = $constraints;
  }
  /**
   * @return InterconnectRemoteLocationConstraints
   */
  public function getConstraints()
  {
    return $this->constraints;
  }
  /**
   * @param string
   */
  public function setContinent($continent)
  {
    $this->continent = $continent;
  }
  /**
   * @return string
   */
  public function getContinent()
  {
    return $this->continent;
  }
  /**
   * @param string
   */
  public function setCreationTimestamp($creationTimestamp)
  {
    $this->creationTimestamp = $creationTimestamp;
  }
  /**
   * @return string
   */
  public function getCreationTimestamp()
  {
    return $this->creationTimestamp;
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
   * @param string
   */
  public function setFacilityProvider($facilityProvider)
  {
    $this->facilityProvider = $facilityProvider;
  }
  /**
   * @return string
   */
  public function getFacilityProvider()
  {
    return $this->facilityProvider;
  }
  /**
   * @param string
   */
  public function setFacilityProviderFacilityId($facilityProviderFacilityId)
  {
    $this->facilityProviderFacilityId = $facilityProviderFacilityId;
  }
  /**
   * @return string
   */
  public function getFacilityProviderFacilityId()
  {
    return $this->facilityProviderFacilityId;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param string
   */
  public function setLacp($lacp)
  {
    $this->lacp = $lacp;
  }
  /**
   * @return string
   */
  public function getLacp()
  {
    return $this->lacp;
  }
  /**
   * @param int
   */
  public function setMaxLagSize100Gbps($maxLagSize100Gbps)
  {
    $this->maxLagSize100Gbps = $maxLagSize100Gbps;
  }
  /**
   * @return int
   */
  public function getMaxLagSize100Gbps()
  {
    return $this->maxLagSize100Gbps;
  }
  /**
   * @param int
   */
  public function setMaxLagSize10Gbps($maxLagSize10Gbps)
  {
    $this->maxLagSize10Gbps = $maxLagSize10Gbps;
  }
  /**
   * @return int
   */
  public function getMaxLagSize10Gbps()
  {
    return $this->maxLagSize10Gbps;
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
  public function setPeeringdbFacilityId($peeringdbFacilityId)
  {
    $this->peeringdbFacilityId = $peeringdbFacilityId;
  }
  /**
   * @return string
   */
  public function getPeeringdbFacilityId()
  {
    return $this->peeringdbFacilityId;
  }
  /**
   * @param InterconnectRemoteLocationPermittedConnections[]
   */
  public function setPermittedConnections($permittedConnections)
  {
    $this->permittedConnections = $permittedConnections;
  }
  /**
   * @return InterconnectRemoteLocationPermittedConnections[]
   */
  public function getPermittedConnections()
  {
    return $this->permittedConnections;
  }
  /**
   * @param string
   */
  public function setRemoteService($remoteService)
  {
    $this->remoteService = $remoteService;
  }
  /**
   * @return string
   */
  public function getRemoteService()
  {
    return $this->remoteService;
  }
  /**
   * @param string
   */
  public function setSelfLink($selfLink)
  {
    $this->selfLink = $selfLink;
  }
  /**
   * @return string
   */
  public function getSelfLink()
  {
    return $this->selfLink;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InterconnectRemoteLocation::class, 'Google_Service_Compute_InterconnectRemoteLocation');
