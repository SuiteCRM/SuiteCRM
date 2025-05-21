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

class GoogleCloudAiplatformV1QueryDeployedModelsResponse extends \Google\Collection
{
  protected $collection_key = 'deployedModels';
  protected $deployedModelRefsType = GoogleCloudAiplatformV1DeployedModelRef::class;
  protected $deployedModelRefsDataType = 'array';
  protected $deployedModelsType = GoogleCloudAiplatformV1DeployedModel::class;
  protected $deployedModelsDataType = 'array';
  /**
   * @var string
   */
  public $nextPageToken;
  /**
   * @var int
   */
  public $totalDeployedModelCount;
  /**
   * @var int
   */
  public $totalEndpointCount;

  /**
   * @param GoogleCloudAiplatformV1DeployedModelRef[]
   */
  public function setDeployedModelRefs($deployedModelRefs)
  {
    $this->deployedModelRefs = $deployedModelRefs;
  }
  /**
   * @return GoogleCloudAiplatformV1DeployedModelRef[]
   */
  public function getDeployedModelRefs()
  {
    return $this->deployedModelRefs;
  }
  /**
   * @param GoogleCloudAiplatformV1DeployedModel[]
   */
  public function setDeployedModels($deployedModels)
  {
    $this->deployedModels = $deployedModels;
  }
  /**
   * @return GoogleCloudAiplatformV1DeployedModel[]
   */
  public function getDeployedModels()
  {
    return $this->deployedModels;
  }
  /**
   * @param string
   */
  public function setNextPageToken($nextPageToken)
  {
    $this->nextPageToken = $nextPageToken;
  }
  /**
   * @return string
   */
  public function getNextPageToken()
  {
    return $this->nextPageToken;
  }
  /**
   * @param int
   */
  public function setTotalDeployedModelCount($totalDeployedModelCount)
  {
    $this->totalDeployedModelCount = $totalDeployedModelCount;
  }
  /**
   * @return int
   */
  public function getTotalDeployedModelCount()
  {
    return $this->totalDeployedModelCount;
  }
  /**
   * @param int
   */
  public function setTotalEndpointCount($totalEndpointCount)
  {
    $this->totalEndpointCount = $totalEndpointCount;
  }
  /**
   * @return int
   */
  public function getTotalEndpointCount()
  {
    return $this->totalEndpointCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1QueryDeployedModelsResponse::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1QueryDeployedModelsResponse');
