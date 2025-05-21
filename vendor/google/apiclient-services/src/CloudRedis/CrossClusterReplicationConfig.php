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

namespace Google\Service\CloudRedis;

class CrossClusterReplicationConfig extends \Google\Collection
{
  protected $collection_key = 'secondaryClusters';
  /**
   * @var string
   */
  public $clusterRole;
  protected $membershipType = Membership::class;
  protected $membershipDataType = '';
  protected $primaryClusterType = RemoteCluster::class;
  protected $primaryClusterDataType = '';
  protected $secondaryClustersType = RemoteCluster::class;
  protected $secondaryClustersDataType = 'array';
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setClusterRole($clusterRole)
  {
    $this->clusterRole = $clusterRole;
  }
  /**
   * @return string
   */
  public function getClusterRole()
  {
    return $this->clusterRole;
  }
  /**
   * @param Membership
   */
  public function setMembership(Membership $membership)
  {
    $this->membership = $membership;
  }
  /**
   * @return Membership
   */
  public function getMembership()
  {
    return $this->membership;
  }
  /**
   * @param RemoteCluster
   */
  public function setPrimaryCluster(RemoteCluster $primaryCluster)
  {
    $this->primaryCluster = $primaryCluster;
  }
  /**
   * @return RemoteCluster
   */
  public function getPrimaryCluster()
  {
    return $this->primaryCluster;
  }
  /**
   * @param RemoteCluster[]
   */
  public function setSecondaryClusters($secondaryClusters)
  {
    $this->secondaryClusters = $secondaryClusters;
  }
  /**
   * @return RemoteCluster[]
   */
  public function getSecondaryClusters()
  {
    return $this->secondaryClusters;
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
class_alias(CrossClusterReplicationConfig::class, 'Google_Service_CloudRedis_CrossClusterReplicationConfig');
