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

class GoogleCloudRunV2BuildpacksBuild extends \Google\Model
{
  /**
   * @var string
   */
  public $baseImage;
  /**
   * @var string
   */
  public $cacheImageUri;
  /**
   * @var bool
   */
  public $enableAutomaticUpdates;
  /**
   * @var string[]
   */
  public $environmentVariables;
  /**
   * @var string
   */
  public $functionTarget;
  /**
   * @var string
   */
  public $projectDescriptor;
  /**
   * @var string
   */
  public $runtime;

  /**
   * @param string
   */
  public function setBaseImage($baseImage)
  {
    $this->baseImage = $baseImage;
  }
  /**
   * @return string
   */
  public function getBaseImage()
  {
    return $this->baseImage;
  }
  /**
   * @param string
   */
  public function setCacheImageUri($cacheImageUri)
  {
    $this->cacheImageUri = $cacheImageUri;
  }
  /**
   * @return string
   */
  public function getCacheImageUri()
  {
    return $this->cacheImageUri;
  }
  /**
   * @param bool
   */
  public function setEnableAutomaticUpdates($enableAutomaticUpdates)
  {
    $this->enableAutomaticUpdates = $enableAutomaticUpdates;
  }
  /**
   * @return bool
   */
  public function getEnableAutomaticUpdates()
  {
    return $this->enableAutomaticUpdates;
  }
  /**
   * @param string[]
   */
  public function setEnvironmentVariables($environmentVariables)
  {
    $this->environmentVariables = $environmentVariables;
  }
  /**
   * @return string[]
   */
  public function getEnvironmentVariables()
  {
    return $this->environmentVariables;
  }
  /**
   * @param string
   */
  public function setFunctionTarget($functionTarget)
  {
    $this->functionTarget = $functionTarget;
  }
  /**
   * @return string
   */
  public function getFunctionTarget()
  {
    return $this->functionTarget;
  }
  /**
   * @param string
   */
  public function setProjectDescriptor($projectDescriptor)
  {
    $this->projectDescriptor = $projectDescriptor;
  }
  /**
   * @return string
   */
  public function getProjectDescriptor()
  {
    return $this->projectDescriptor;
  }
  /**
   * @param string
   */
  public function setRuntime($runtime)
  {
    $this->runtime = $runtime;
  }
  /**
   * @return string
   */
  public function getRuntime()
  {
    return $this->runtime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRunV2BuildpacksBuild::class, 'Google_Service_CloudRun_GoogleCloudRunV2BuildpacksBuild');
