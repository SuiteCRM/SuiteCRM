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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1CustomJobSpec extends \Google\Collection
{
  protected $collection_key = 'workerPoolSpecs';
  protected $baseOutputDirectoryType = GoogleCloudAiplatformV1GcsDestination::class;
  protected $baseOutputDirectoryDataType = '';
  /**
   * @var bool
   */
  public $enableDashboardAccess;
  /**
   * @var bool
   */
  public $enableWebAccess;
  /**
   * @var string
   */
  public $experiment;
  /**
   * @var string
   */
  public $experimentRun;
  /**
   * @var string[]
   */
  public $models;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $persistentResourceId;
  /**
   * @var string
   */
  public $protectedArtifactLocationId;
  /**
   * @var string[]
   */
  public $reservedIpRanges;
  protected $schedulingType = GoogleCloudAiplatformV1Scheduling::class;
  protected $schedulingDataType = '';
  /**
   * @var string
   */
  public $serviceAccount;
  /**
   * @var string
   */
  public $tensorboard;
  protected $workerPoolSpecsType = GoogleCloudAiplatformV1WorkerPoolSpec::class;
  protected $workerPoolSpecsDataType = 'array';

  /**
   * @param GoogleCloudAiplatformV1GcsDestination
   */
  public function setBaseOutputDirectory(GoogleCloudAiplatformV1GcsDestination $baseOutputDirectory)
  {
    $this->baseOutputDirectory = $baseOutputDirectory;
  }
  /**
   * @return GoogleCloudAiplatformV1GcsDestination
   */
  public function getBaseOutputDirectory()
  {
    return $this->baseOutputDirectory;
  }
  /**
   * @param bool
   */
  public function setEnableDashboardAccess($enableDashboardAccess)
  {
    $this->enableDashboardAccess = $enableDashboardAccess;
  }
  /**
   * @return bool
   */
  public function getEnableDashboardAccess()
  {
    return $this->enableDashboardAccess;
  }
  /**
   * @param bool
   */
  public function setEnableWebAccess($enableWebAccess)
  {
    $this->enableWebAccess = $enableWebAccess;
  }
  /**
   * @return bool
   */
  public function getEnableWebAccess()
  {
    return $this->enableWebAccess;
  }
  /**
   * @param string
   */
  public function setExperiment($experiment)
  {
    $this->experiment = $experiment;
  }
  /**
   * @return string
   */
  public function getExperiment()
  {
    return $this->experiment;
  }
  /**
   * @param string
   */
  public function setExperimentRun($experimentRun)
  {
    $this->experimentRun = $experimentRun;
  }
  /**
   * @return string
   */
  public function getExperimentRun()
  {
    return $this->experimentRun;
  }
  /**
   * @param string[]
   */
  public function setModels($models)
  {
    $this->models = $models;
  }
  /**
   * @return string[]
   */
  public function getModels()
  {
    return $this->models;
  }
  /**
   * @param string
   */
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param string
   */
  public function setPersistentResourceId($persistentResourceId)
  {
    $this->persistentResourceId = $persistentResourceId;
  }
  /**
   * @return string
   */
  public function getPersistentResourceId()
  {
    return $this->persistentResourceId;
  }
  /**
   * @param string
   */
  public function setProtectedArtifactLocationId($protectedArtifactLocationId)
  {
    $this->protectedArtifactLocationId = $protectedArtifactLocationId;
  }
  /**
   * @return string
   */
  public function getProtectedArtifactLocationId()
  {
    return $this->protectedArtifactLocationId;
  }
  /**
   * @param string[]
   */
  public function setReservedIpRanges($reservedIpRanges)
  {
    $this->reservedIpRanges = $reservedIpRanges;
  }
  /**
   * @return string[]
   */
  public function getReservedIpRanges()
  {
    return $this->reservedIpRanges;
  }
  /**
   * @param GoogleCloudAiplatformV1Scheduling
   */
  public function setScheduling(GoogleCloudAiplatformV1Scheduling $scheduling)
  {
    $this->scheduling = $scheduling;
  }
  /**
   * @return GoogleCloudAiplatformV1Scheduling
   */
  public function getScheduling()
  {
    return $this->scheduling;
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
  public function setTensorboard($tensorboard)
  {
    $this->tensorboard = $tensorboard;
  }
  /**
   * @return string
   */
  public function getTensorboard()
  {
    return $this->tensorboard;
  }
  /**
   * @param GoogleCloudAiplatformV1WorkerPoolSpec[]
   */
  public function setWorkerPoolSpecs($workerPoolSpecs)
  {
    $this->workerPoolSpecs = $workerPoolSpecs;
  }
  /**
   * @return GoogleCloudAiplatformV1WorkerPoolSpec[]
   */
  public function getWorkerPoolSpecs()
  {
    return $this->workerPoolSpecs;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1CustomJobSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1CustomJobSpec');
