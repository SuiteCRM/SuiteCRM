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

namespace Google\Service\CloudRun;

class GoogleCloudRunV2RevisionTemplate extends \Google\Collection
{
  protected $collection_key = 'volumes';
  /**
   * @var string[]
   */
  public $annotations;
  protected $containersType = GoogleCloudRunV2Container::class;
  protected $containersDataType = 'array';
  /**
   * @var string
   */
  public $encryptionKey;
  /**
   * @var string
   */
  public $encryptionKeyRevocationAction;
  /**
   * @var string
   */
  public $encryptionKeyShutdownDuration;
  /**
   * @var string
   */
  public $executionEnvironment;
  /**
   * @var bool
   */
  public $healthCheckDisabled;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var int
   */
  public $maxInstanceRequestConcurrency;
  protected $nodeSelectorType = GoogleCloudRunV2NodeSelector::class;
  protected $nodeSelectorDataType = '';
  /**
   * @var string
   */
  public $revision;
  protected $scalingType = GoogleCloudRunV2RevisionScaling::class;
  protected $scalingDataType = '';
  /**
   * @var string
   */
  public $serviceAccount;
  protected $serviceMeshType = GoogleCloudRunV2ServiceMesh::class;
  protected $serviceMeshDataType = '';
  /**
   * @var bool
   */
  public $sessionAffinity;
  /**
   * @var string
   */
  public $timeout;
  protected $volumesType = GoogleCloudRunV2Volume::class;
  protected $volumesDataType = 'array';
  protected $vpcAccessType = GoogleCloudRunV2VpcAccess::class;
  protected $vpcAccessDataType = '';

  /**
   * @param string[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return string[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param GoogleCloudRunV2Container[]
   */
  public function setContainers($containers)
  {
    $this->containers = $containers;
  }
  /**
   * @return GoogleCloudRunV2Container[]
   */
  public function getContainers()
  {
    return $this->containers;
  }
  /**
   * @param string
   */
  public function setEncryptionKey($encryptionKey)
  {
    $this->encryptionKey = $encryptionKey;
  }
  /**
   * @return string
   */
  public function getEncryptionKey()
  {
    return $this->encryptionKey;
  }
  /**
   * @param string
   */
  public function setEncryptionKeyRevocationAction($encryptionKeyRevocationAction)
  {
    $this->encryptionKeyRevocationAction = $encryptionKeyRevocationAction;
  }
  /**
   * @return string
   */
  public function getEncryptionKeyRevocationAction()
  {
    return $this->encryptionKeyRevocationAction;
  }
  /**
   * @param string
   */
  public function setEncryptionKeyShutdownDuration($encryptionKeyShutdownDuration)
  {
    $this->encryptionKeyShutdownDuration = $encryptionKeyShutdownDuration;
  }
  /**
   * @return string
   */
  public function getEncryptionKeyShutdownDuration()
  {
    return $this->encryptionKeyShutdownDuration;
  }
  /**
   * @param string
   */
  public function setExecutionEnvironment($executionEnvironment)
  {
    $this->executionEnvironment = $executionEnvironment;
  }
  /**
   * @return string
   */
  public function getExecutionEnvironment()
  {
    return $this->executionEnvironment;
  }
  /**
   * @param bool
   */
  public function setHealthCheckDisabled($healthCheckDisabled)
  {
    $this->healthCheckDisabled = $healthCheckDisabled;
  }
  /**
   * @return bool
   */
  public function getHealthCheckDisabled()
  {
    return $this->healthCheckDisabled;
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
   * @param int
   */
  public function setMaxInstanceRequestConcurrency($maxInstanceRequestConcurrency)
  {
    $this->maxInstanceRequestConcurrency = $maxInstanceRequestConcurrency;
  }
  /**
   * @return int
   */
  public function getMaxInstanceRequestConcurrency()
  {
    return $this->maxInstanceRequestConcurrency;
  }
  /**
   * @param GoogleCloudRunV2NodeSelector
   */
  public function setNodeSelector(GoogleCloudRunV2NodeSelector $nodeSelector)
  {
    $this->nodeSelector = $nodeSelector;
  }
  /**
   * @return GoogleCloudRunV2NodeSelector
   */
  public function getNodeSelector()
  {
    return $this->nodeSelector;
  }
  /**
   * @param string
   */
  public function setRevision($revision)
  {
    $this->revision = $revision;
  }
  /**
   * @return string
   */
  public function getRevision()
  {
    return $this->revision;
  }
  /**
   * @param GoogleCloudRunV2RevisionScaling
   */
  public function setScaling(GoogleCloudRunV2RevisionScaling $scaling)
  {
    $this->scaling = $scaling;
  }
  /**
   * @return GoogleCloudRunV2RevisionScaling
   */
  public function getScaling()
  {
    return $this->scaling;
  }
  /**
   * @param string
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return string
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param GoogleCloudRunV2ServiceMesh
   */
  public function setServiceMesh(GoogleCloudRunV2ServiceMesh $serviceMesh)
  {
    $this->serviceMesh = $serviceMesh;
  }
  /**
   * @return GoogleCloudRunV2ServiceMesh
   */
  public function getServiceMesh()
  {
    return $this->serviceMesh;
  }
  /**
   * @param bool
   */
  public function setSessionAffinity($sessionAffinity)
  {
    $this->sessionAffinity = $sessionAffinity;
  }
  /**
   * @return bool
   */
  public function getSessionAffinity()
  {
    return $this->sessionAffinity;
  }
  /**
   * @param string
   */
  public function setTimeout($timeout)
  {
    $this->timeout = $timeout;
  }
  /**
   * @return string
   */
  public function getTimeout()
  {
    return $this->timeout;
  }
  /**
   * @param GoogleCloudRunV2Volume[]
   */
  public function setVolumes($volumes)
  {
    $this->volumes = $volumes;
  }
  /**
   * @return GoogleCloudRunV2Volume[]
   */
  public function getVolumes()
  {
    return $this->volumes;
  }
  /**
   * @param GoogleCloudRunV2VpcAccess
   */
  public function setVpcAccess(GoogleCloudRunV2VpcAccess $vpcAccess)
  {
    $this->vpcAccess = $vpcAccess;
  }
  /**
   * @return GoogleCloudRunV2VpcAccess
   */
  public function getVpcAccess()
  {
    return $this->vpcAccess;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRunV2RevisionTemplate::class, 'Google_Service_CloudRun_GoogleCloudRunV2RevisionTemplate');
