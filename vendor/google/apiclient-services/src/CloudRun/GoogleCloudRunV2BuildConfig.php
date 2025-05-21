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

class GoogleCloudRunV2BuildConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $baseImage;
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
  public $imageUri;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string
   */
  public $sourceLocation;
  /**
   * @var string
   */
  public $workerPool;

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
  public function setImageUri($imageUri)
  {
    $this->imageUri = $imageUri;
  }
  /**
   * @return string
   */
  public function getImageUri()
  {
    return $this->imageUri;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
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
   * @param string
   */
  public function setSourceLocation($sourceLocation)
  {
    $this->sourceLocation = $sourceLocation;
  }
  /**
   * @return string
   */
  public function getSourceLocation()
  {
    return $this->sourceLocation;
  }
  /**
   * @param string
   */
  public function setWorkerPool($workerPool)
  {
    $this->workerPool = $workerPool;
  }
  /**
   * @return string
   */
  public function getWorkerPool()
  {
    return $this->workerPool;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRunV2BuildConfig::class, 'Google_Service_CloudRun_GoogleCloudRunV2BuildConfig');
