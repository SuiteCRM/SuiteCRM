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

class SapDiscoveryComponentDatabaseProperties extends \Google\Model
{
  /**
   * @var string
   */
  public $databaseSid;
  /**
   * @var string
   */
  public $databaseType;
  /**
   * @var string
   */
  public $databaseVersion;
  /**
   * @var string
   */
  public $instanceNumber;
  /**
   * @var string
   */
  public $landscapeId;
  /**
   * @var string
   */
  public $primaryInstanceUri;
  /**
   * @var string
   */
  public $sharedNfsUri;

  /**
   * @param string
   */
  public function setDatabaseSid($databaseSid)
  {
    $this->databaseSid = $databaseSid;
  }
  /**
   * @return string
   */
  public function getDatabaseSid()
  {
    return $this->databaseSid;
  }
  /**
   * @param string
   */
  public function setDatabaseType($databaseType)
  {
    $this->databaseType = $databaseType;
  }
  /**
   * @return string
   */
  public function getDatabaseType()
  {
    return $this->databaseType;
  }
  /**
   * @param string
   */
  public function setDatabaseVersion($databaseVersion)
  {
    $this->databaseVersion = $databaseVersion;
  }
  /**
   * @return string
   */
  public function getDatabaseVersion()
  {
    return $this->databaseVersion;
  }
  /**
   * @param string
   */
  public function setInstanceNumber($instanceNumber)
  {
    $this->instanceNumber = $instanceNumber;
  }
  /**
   * @return string
   */
  public function getInstanceNumber()
  {
    return $this->instanceNumber;
  }
  /**
   * @param string
   */
  public function setLandscapeId($landscapeId)
  {
    $this->landscapeId = $landscapeId;
  }
  /**
   * @return string
   */
  public function getLandscapeId()
  {
    return $this->landscapeId;
  }
  /**
   * @param string
   */
  public function setPrimaryInstanceUri($primaryInstanceUri)
  {
    $this->primaryInstanceUri = $primaryInstanceUri;
  }
  /**
   * @return string
   */
  public function getPrimaryInstanceUri()
  {
    return $this->primaryInstanceUri;
  }
  /**
   * @param string
   */
  public function setSharedNfsUri($sharedNfsUri)
  {
    $this->sharedNfsUri = $sharedNfsUri;
  }
  /**
   * @return string
   */
  public function getSharedNfsUri()
  {
    return $this->sharedNfsUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SapDiscoveryComponentDatabaseProperties::class, 'Google_Service_WorkloadManager_SapDiscoveryComponentDatabaseProperties');
