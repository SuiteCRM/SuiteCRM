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

namespace Google\Service\SQLAdmin;

class ReplicationCluster extends \Google\Model
{
  /**
   * @var bool
   */
  public $drReplica;
  /**
   * @var string
   */
  public $failoverDrReplicaName;
  /**
   * @var string
   */
  public $psaWriteEndpoint;

  /**
   * @param bool
   */
  public function setDrReplica($drReplica)
  {
    $this->drReplica = $drReplica;
  }
  /**
   * @return bool
   */
  public function getDrReplica()
  {
    return $this->drReplica;
  }
  /**
   * @param string
   */
  public function setFailoverDrReplicaName($failoverDrReplicaName)
  {
    $this->failoverDrReplicaName = $failoverDrReplicaName;
  }
  /**
   * @return string
   */
  public function getFailoverDrReplicaName()
  {
    return $this->failoverDrReplicaName;
  }
  /**
   * @param string
   */
  public function setPsaWriteEndpoint($psaWriteEndpoint)
  {
    $this->psaWriteEndpoint = $psaWriteEndpoint;
  }
  /**
   * @return string
   */
  public function getPsaWriteEndpoint()
  {
    return $this->psaWriteEndpoint;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ReplicationCluster::class, 'Google_Service_SQLAdmin_ReplicationCluster');
