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

namespace Google\Service\GKEHub;

class PolicyControllerPolicyControllerDeploymentConfig extends \Google\Collection
{
  protected $collection_key = 'podTolerations';
  protected $containerResourcesType = PolicyControllerResourceRequirements::class;
  protected $containerResourcesDataType = '';
  /**
   * @var string
   */
  public $podAffinity;
  /**
   * @var bool
   */
  public $podAntiAffinity;
  protected $podTolerationsType = PolicyControllerToleration::class;
  protected $podTolerationsDataType = 'array';
  /**
   * @var string
   */
  public $replicaCount;

  /**
   * @param PolicyControllerResourceRequirements
   */
  public function setContainerResources(PolicyControllerResourceRequirements $containerResources)
  {
    $this->containerResources = $containerResources;
  }
  /**
   * @return PolicyControllerResourceRequirements
   */
  public function getContainerResources()
  {
    return $this->containerResources;
  }
  /**
   * @param string
   */
  public function setPodAffinity($podAffinity)
  {
    $this->podAffinity = $podAffinity;
  }
  /**
   * @return string
   */
  public function getPodAffinity()
  {
    return $this->podAffinity;
  }
  /**
   * @param bool
   */
  public function setPodAntiAffinity($podAntiAffinity)
  {
    $this->podAntiAffinity = $podAntiAffinity;
  }
  /**
   * @return bool
   */
  public function getPodAntiAffinity()
  {
    return $this->podAntiAffinity;
  }
  /**
   * @param PolicyControllerToleration[]
   */
  public function setPodTolerations($podTolerations)
  {
    $this->podTolerations = $podTolerations;
  }
  /**
   * @return PolicyControllerToleration[]
   */
  public function getPodTolerations()
  {
    return $this->podTolerations;
  }
  /**
   * @param string
   */
  public function setReplicaCount($replicaCount)
  {
    $this->replicaCount = $replicaCount;
  }
  /**
   * @return string
   */
  public function getReplicaCount()
  {
    return $this->replicaCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PolicyControllerPolicyControllerDeploymentConfig::class, 'Google_Service_GKEHub_PolicyControllerPolicyControllerDeploymentConfig');
