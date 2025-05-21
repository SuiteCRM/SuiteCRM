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

class GoogleCloudAiplatformV1PredictResponse extends \Google\Collection
{
  protected $collection_key = 'predictions';
  /**
   * @var string
   */
  public $deployedModelId;
  /**
   * @var array
   */
  public $metadata;
  /**
   * @var string
   */
  public $model;
  /**
   * @var string
   */
  public $modelDisplayName;
  /**
   * @var string
   */
  public $modelVersionId;
  /**
   * @var array[]
   */
  public $predictions;

  /**
   * @param string
   */
  public function setDeployedModelId($deployedModelId)
  {
    $this->deployedModelId = $deployedModelId;
  }
  /**
   * @return string
   */
  public function getDeployedModelId()
  {
    return $this->deployedModelId;
  }
  /**
   * @param array
   */
  public function setMetadata($metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return array
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param string
   */
  public function setModel($model)
  {
    $this->model = $model;
  }
  /**
   * @return string
   */
  public function getModel()
  {
    return $this->model;
  }
  /**
   * @param string
   */
  public function setModelDisplayName($modelDisplayName)
  {
    $this->modelDisplayName = $modelDisplayName;
  }
  /**
   * @return string
   */
  public function getModelDisplayName()
  {
    return $this->modelDisplayName;
  }
  /**
   * @param string
   */
  public function setModelVersionId($modelVersionId)
  {
    $this->modelVersionId = $modelVersionId;
  }
  /**
   * @return string
   */
  public function getModelVersionId()
  {
    return $this->modelVersionId;
  }
  /**
   * @param array[]
   */
  public function setPredictions($predictions)
  {
    $this->predictions = $predictions;
  }
  /**
   * @return array[]
   */
  public function getPredictions()
  {
    return $this->predictions;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PredictResponse::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PredictResponse');
