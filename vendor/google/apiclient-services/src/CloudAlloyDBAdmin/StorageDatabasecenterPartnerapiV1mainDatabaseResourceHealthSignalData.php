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

namespace Google\Service\CloudAlloyDBAdmin;

class StorageDatabasecenterPartnerapiV1mainDatabaseResourceHealthSignalData extends \Google\Collection
{
  protected $collection_key = 'compliance';
  /**
   * @var array[]
   */
  public $additionalMetadata;
  protected $complianceType = StorageDatabasecenterPartnerapiV1mainCompliance::class;
  protected $complianceDataType = 'array';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $eventTime;
  /**
   * @var string
   */
  public $externalUri;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $provider;
  /**
   * @var string
   */
  public $resourceContainer;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $signalClass;
  /**
   * @var string
   */
  public $signalId;
  /**
   * @var string
   */
  public $signalSeverity;
  /**
   * @var string
   */
  public $signalType;
  /**
   * @var string
   */
  public $state;

  /**
   * @param array[]
   */
  public function setAdditionalMetadata($additionalMetadata)
  {
    $this->additionalMetadata = $additionalMetadata;
  }
  /**
   * @return array[]
   */
  public function getAdditionalMetadata()
  {
    return $this->additionalMetadata;
  }
  /**
   * @param StorageDatabasecenterPartnerapiV1mainCompliance[]
   */
  public function setCompliance($compliance)
  {
    $this->compliance = $compliance;
  }
  /**
   * @return StorageDatabasecenterPartnerapiV1mainCompliance[]
   */
  public function getCompliance()
  {
    return $this->compliance;
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
  public function setEventTime($eventTime)
  {
    $this->eventTime = $eventTime;
  }
  /**
   * @return string
   */
  public function getEventTime()
  {
    return $this->eventTime;
  }
  /**
   * @param string
   */
  public function setExternalUri($externalUri)
  {
    $this->externalUri = $externalUri;
  }
  /**
   * @return string
   */
  public function getExternalUri()
  {
    return $this->externalUri;
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
  public function setProvider($provider)
  {
    $this->provider = $provider;
  }
  /**
   * @return string
   */
  public function getProvider()
  {
    return $this->provider;
  }
  /**
   * @param string
   */
  public function setResourceContainer($resourceContainer)
  {
    $this->resourceContainer = $resourceContainer;
  }
  /**
   * @return string
   */
  public function getResourceContainer()
  {
    return $this->resourceContainer;
  }
  /**
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string
   */
  public function setSignalClass($signalClass)
  {
    $this->signalClass = $signalClass;
  }
  /**
   * @return string
   */
  public function getSignalClass()
  {
    return $this->signalClass;
  }
  /**
   * @param string
   */
  public function setSignalId($signalId)
  {
    $this->signalId = $signalId;
  }
  /**
   * @return string
   */
  public function getSignalId()
  {
    return $this->signalId;
  }
  /**
   * @param string
   */
  public function setSignalSeverity($signalSeverity)
  {
    $this->signalSeverity = $signalSeverity;
  }
  /**
   * @return string
   */
  public function getSignalSeverity()
  {
    return $this->signalSeverity;
  }
  /**
   * @param string
   */
  public function setSignalType($signalType)
  {
    $this->signalType = $signalType;
  }
  /**
   * @return string
   */
  public function getSignalType()
  {
    return $this->signalType;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StorageDatabasecenterPartnerapiV1mainDatabaseResourceHealthSignalData::class, 'Google_Service_CloudAlloyDBAdmin_StorageDatabasecenterPartnerapiV1mainDatabaseResourceHealthSignalData');
