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

namespace Google\Service\GKEOnPrem;

class VmwareNodeConfig extends \Google\Collection
{
  protected $collection_key = 'taints';
  /**
   * @var string
   */
  public $bootDiskSizeGb;
  /**
   * @var string
   */
  public $cpus;
  /**
   * @var bool
   */
  public $enableLoadBalancer;
  /**
   * @var string
   */
  public $image;
  /**
   * @var string
   */
  public $imageType;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $memoryMb;
  /**
   * @var string
   */
  public $replicas;
  protected $taintsType = NodeTaint::class;
  protected $taintsDataType = 'array';
  protected $vsphereConfigType = VmwareVsphereConfig::class;
  protected $vsphereConfigDataType = '';

  /**
   * @param string
   */
  public function setBootDiskSizeGb($bootDiskSizeGb)
  {
    $this->bootDiskSizeGb = $bootDiskSizeGb;
  }
  /**
   * @return string
   */
  public function getBootDiskSizeGb()
  {
    return $this->bootDiskSizeGb;
  }
  /**
   * @param string
   */
  public function setCpus($cpus)
  {
    $this->cpus = $cpus;
  }
  /**
   * @return string
   */
  public function getCpus()
  {
    return $this->cpus;
  }
  /**
   * @param bool
   */
  public function setEnableLoadBalancer($enableLoadBalancer)
  {
    $this->enableLoadBalancer = $enableLoadBalancer;
  }
  /**
   * @return bool
   */
  public function getEnableLoadBalancer()
  {
    return $this->enableLoadBalancer;
  }
  /**
   * @param string
   */
  public function setImage($image)
  {
    $this->image = $image;
  }
  /**
   * @return string
   */
  public function getImage()
  {
    return $this->image;
  }
  /**
   * @param string
   */
  public function setImageType($imageType)
  {
    $this->imageType = $imageType;
  }
  /**
   * @return string
   */
  public function getImageType()
  {
    return $this->imageType;
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
   * @param string
   */
  public function setMemoryMb($memoryMb)
  {
    $this->memoryMb = $memoryMb;
  }
  /**
   * @return string
   */
  public function getMemoryMb()
  {
    return $this->memoryMb;
  }
  /**
   * @param string
   */
  public function setReplicas($replicas)
  {
    $this->replicas = $replicas;
  }
  /**
   * @return string
   */
  public function getReplicas()
  {
    return $this->replicas;
  }
  /**
   * @param NodeTaint[]
   */
  public function setTaints($taints)
  {
    $this->taints = $taints;
  }
  /**
   * @return NodeTaint[]
   */
  public function getTaints()
  {
    return $this->taints;
  }
  /**
   * @param VmwareVsphereConfig
   */
  public function setVsphereConfig(VmwareVsphereConfig $vsphereConfig)
  {
    $this->vsphereConfig = $vsphereConfig;
  }
  /**
   * @return VmwareVsphereConfig
   */
  public function getVsphereConfig()
  {
    return $this->vsphereConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareNodeConfig::class, 'Google_Service_GKEOnPrem_VmwareNodeConfig');
