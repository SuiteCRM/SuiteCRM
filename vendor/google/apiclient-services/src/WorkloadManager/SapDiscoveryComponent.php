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

class SapDiscoveryComponent extends \Google\Collection
{
  protected $collection_key = 'resources';
  protected $applicationPropertiesType = SapDiscoveryComponentApplicationProperties::class;
  protected $applicationPropertiesDataType = '';
  protected $databasePropertiesType = SapDiscoveryComponentDatabaseProperties::class;
  protected $databasePropertiesDataType = '';
  /**
   * @var string[]
   */
  public $haHosts;
  /**
   * @var string
   */
  public $hostProject;
  /**
   * @var string
   */
  public $region;
  protected $replicationSitesType = SapDiscoveryComponentReplicationSite::class;
  protected $replicationSitesDataType = 'array';
  protected $resourcesType = SapDiscoveryResource::class;
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
   * @param SapDiscoveryComponentApplicationProperties
   */
  public function setApplicationProperties(SapDiscoveryComponentApplicationProperties $applicationProperties)
  {
    $this->applicationProperties = $applicationProperties;
  }
  /**
   * @return SapDiscoveryComponentApplicationProperties
   */
  public function getApplicationProperties()
  {
    return $this->applicationProperties;
  }
  /**
   * @param SapDiscoveryComponentDatabaseProperties
   */
  public function setDatabaseProperties(SapDiscoveryComponentDatabaseProperties $databaseProperties)
  {
    $this->databaseProperties = $databaseProperties;
  }
  /**
   * @return SapDiscoveryComponentDatabaseProperties
   */
  public function getDatabaseProperties()
  {
    return $this->databaseProperties;
  }
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
   * @param string
   */
  public function setHostProject($hostProject)
  {
    $this->hostProject = $hostProject;
  }
  /**
   * @return string
   */
  public function getHostProject()
  {
    return $this->hostProject;
  }
  /**
   * @param string
   */
  public function setRegion($region)
  {
    $this->region = $region;
  }
  /**
   * @return string
   */
  public function getRegion()
  {
    return $this->region;
  }
  /**
   * @param SapDiscoveryComponentReplicationSite[]
   */
  public function setReplicationSites($replicationSites)
  {
    $this->replicationSites = $replicationSites;
  }
  /**
   * @return SapDiscoveryComponentReplicationSite[]
   */
  public function getReplicationSites()
  {
    return $this->replicationSites;
  }
  /**
   * @param SapDiscoveryResource[]
   */
  public function setResources($resources)
  {
    $this->resources = $resources;
  }
  /**
   * @return SapDiscoveryResource[]
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
class_alias(SapDiscoveryComponent::class, 'Google_Service_WorkloadManager_SapDiscoveryComponent');
