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

class NodeKubeletConfig extends \Google\Collection
{
  protected $collection_key = 'allowedUnsafeSysctls';
  /**
   * @var string[]
   */
  public $allowedUnsafeSysctls;
  /**
   * @var int
   */
  public $containerLogMaxFiles;
  /**
   * @var string
   */
  public $containerLogMaxSize;
  /**
   * @var bool
   */
  public $cpuCfsQuota;
  /**
   * @var string
   */
  public $cpuCfsQuotaPeriod;
  /**
   * @var string
   */
  public $cpuManagerPolicy;
  /**
   * @var int
   */
  public $imageGcHighThresholdPercent;
  /**
   * @var int
   */
  public $imageGcLowThresholdPercent;
  /**
   * @var string
   */
  public $imageMaximumGcAge;
  /**
   * @var string
   */
  public $imageMinimumGcAge;
  /**
   * @var bool
   */
  public $insecureKubeletReadonlyPortEnabled;
  /**
   * @var string
   */
  public $podPidsLimit;

  /**
   * @param string[]
   */
  public function setAllowedUnsafeSysctls($allowedUnsafeSysctls)
  {
    $this->allowedUnsafeSysctls = $allowedUnsafeSysctls;
  }
  /**
   * @return string[]
   */
  public function getAllowedUnsafeSysctls()
  {
    return $this->allowedUnsafeSysctls;
  }
  /**
   * @param int
   */
  public function setContainerLogMaxFiles($containerLogMaxFiles)
  {
    $this->containerLogMaxFiles = $containerLogMaxFiles;
  }
  /**
   * @return int
   */
  public function getContainerLogMaxFiles()
  {
    return $this->containerLogMaxFiles;
  }
  /**
   * @param string
   */
  public function setContainerLogMaxSize($containerLogMaxSize)
  {
    $this->containerLogMaxSize = $containerLogMaxSize;
  }
  /**
   * @return string
   */
  public function getContainerLogMaxSize()
  {
    return $this->containerLogMaxSize;
  }
  /**
   * @param bool
   */
  public function setCpuCfsQuota($cpuCfsQuota)
  {
    $this->cpuCfsQuota = $cpuCfsQuota;
  }
  /**
   * @return bool
   */
  public function getCpuCfsQuota()
  {
    return $this->cpuCfsQuota;
  }
  /**
   * @param string
   */
  public function setCpuCfsQuotaPeriod($cpuCfsQuotaPeriod)
  {
    $this->cpuCfsQuotaPeriod = $cpuCfsQuotaPeriod;
  }
  /**
   * @return string
   */
  public function getCpuCfsQuotaPeriod()
  {
    return $this->cpuCfsQuotaPeriod;
  }
  /**
   * @param string
   */
  public function setCpuManagerPolicy($cpuManagerPolicy)
  {
    $this->cpuManagerPolicy = $cpuManagerPolicy;
  }
  /**
   * @return string
   */
  public function getCpuManagerPolicy()
  {
    return $this->cpuManagerPolicy;
  }
  /**
   * @param int
   */
  public function setImageGcHighThresholdPercent($imageGcHighThresholdPercent)
  {
    $this->imageGcHighThresholdPercent = $imageGcHighThresholdPercent;
  }
  /**
   * @return int
   */
  public function getImageGcHighThresholdPercent()
  {
    return $this->imageGcHighThresholdPercent;
  }
  /**
   * @param int
   */
  public function setImageGcLowThresholdPercent($imageGcLowThresholdPercent)
  {
    $this->imageGcLowThresholdPercent = $imageGcLowThresholdPercent;
  }
  /**
   * @return int
   */
  public function getImageGcLowThresholdPercent()
  {
    return $this->imageGcLowThresholdPercent;
  }
  /**
   * @param string
   */
  public function setImageMaximumGcAge($imageMaximumGcAge)
  {
    $this->imageMaximumGcAge = $imageMaximumGcAge;
  }
  /**
   * @return string
   */
  public function getImageMaximumGcAge()
  {
    return $this->imageMaximumGcAge;
  }
  /**
   * @param string
   */
  public function setImageMinimumGcAge($imageMinimumGcAge)
  {
    $this->imageMinimumGcAge = $imageMinimumGcAge;
  }
  /**
   * @return string
   */
  public function getImageMinimumGcAge()
  {
    return $this->imageMinimumGcAge;
  }
  /**
   * @param bool
   */
  public function setInsecureKubeletReadonlyPortEnabled($insecureKubeletReadonlyPortEnabled)
  {
    $this->insecureKubeletReadonlyPortEnabled = $insecureKubeletReadonlyPortEnabled;
  }
  /**
   * @return bool
   */
  public function getInsecureKubeletReadonlyPortEnabled()
  {
    return $this->insecureKubeletReadonlyPortEnabled;
  }
  /**
   * @param string
   */
  public function setPodPidsLimit($podPidsLimit)
  {
    $this->podPidsLimit = $podPidsLimit;
  }
  /**
   * @return string
   */
  public function getPodPidsLimit()
  {
    return $this->podPidsLimit;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(NodeKubeletConfig::class, 'Google_Service_Container_NodeKubeletConfig');
