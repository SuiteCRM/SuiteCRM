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

namespace Google\Service\Container;

class UserManagedKeysConfig extends \Google\Collection
{
  protected $collection_key = 'serviceAccountVerificationKeys';
  /**
   * @var string
   */
  public $aggregationCa;
  /**
   * @var string
   */
  public $clusterCa;
  /**
   * @var string
   */
  public $controlPlaneDiskEncryptionKey;
  /**
   * @var string
   */
  public $etcdApiCa;
  /**
   * @var string
   */
  public $etcdPeerCa;
  /**
   * @var string
   */
  public $gkeopsEtcdBackupEncryptionKey;
  /**
   * @var string[]
   */
  public $serviceAccountSigningKeys;
  /**
   * @var string[]
   */
  public $serviceAccountVerificationKeys;

  /**
   * @param string
   */
  public function setAggregationCa($aggregationCa)
  {
    $this->aggregationCa = $aggregationCa;
  }
  /**
   * @return string
   */
  public function getAggregationCa()
  {
    return $this->aggregationCa;
  }
  /**
   * @param string
   */
  public function setClusterCa($clusterCa)
  {
    $this->clusterCa = $clusterCa;
  }
  /**
   * @return string
   */
  public function getClusterCa()
  {
    return $this->clusterCa;
  }
  /**
   * @param string
   */
  public function setControlPlaneDiskEncryptionKey($controlPlaneDiskEncryptionKey)
  {
    $this->controlPlaneDiskEncryptionKey = $controlPlaneDiskEncryptionKey;
  }
  /**
   * @return string
   */
  public function getControlPlaneDiskEncryptionKey()
  {
    return $this->controlPlaneDiskEncryptionKey;
  }
  /**
   * @param string
   */
  public function setEtcdApiCa($etcdApiCa)
  {
    $this->etcdApiCa = $etcdApiCa;
  }
  /**
   * @return string
   */
  public function getEtcdApiCa()
  {
    return $this->etcdApiCa;
  }
  /**
   * @param string
   */
  public function setEtcdPeerCa($etcdPeerCa)
  {
    $this->etcdPeerCa = $etcdPeerCa;
  }
  /**
   * @return string
   */
  public function getEtcdPeerCa()
  {
    return $this->etcdPeerCa;
  }
  /**
   * @param string
   */
  public function setGkeopsEtcdBackupEncryptionKey($gkeopsEtcdBackupEncryptionKey)
  {
    $this->gkeopsEtcdBackupEncryptionKey = $gkeopsEtcdBackupEncryptionKey;
  }
  /**
   * @return string
   */
  public function getGkeopsEtcdBackupEncryptionKey()
  {
    return $this->gkeopsEtcdBackupEncryptionKey;
  }
  /**
   * @param string[]
   */
  public function setServiceAccountSigningKeys($serviceAccountSigningKeys)
  {
    $this->serviceAccountSigningKeys = $serviceAccountSigningKeys;
  }
  /**
   * @return string[]
   */
  public function getServiceAccountSigningKeys()
  {
    return $this->serviceAccountSigningKeys;
  }
  /**
   * @param string[]
   */
  public function setServiceAccountVerificationKeys($serviceAccountVerificationKeys)
  {
    $this->serviceAccountVerificationKeys = $serviceAccountVerificationKeys;
  }
  /**
   * @return string[]
   */
  public function getServiceAccountVerificationKeys()
  {
    return $this->serviceAccountVerificationKeys;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UserManagedKeysConfig::class, 'Google_Service_Container_UserManagedKeysConfig');
