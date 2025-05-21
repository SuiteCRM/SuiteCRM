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

class GoogleCloudAiplatformV1SchemaPredictionResult extends \Google\Model
{
  protected $errorType = GoogleCloudAiplatformV1SchemaPredictionResultError::class;
  protected $errorDataType = '';
  /**
   * @var array[]
   */
  public $instance;
  /**
   * @var string
   */
  public $key;
  /**
   * @var array
   */
  public $prediction;

  /**
   * @param GoogleCloudAiplatformV1SchemaPredictionResultError
   */
  public function setError(GoogleCloudAiplatformV1SchemaPredictionResultError $error)
  {
    $this->error = $error;
  }
  /**
   * @return GoogleCloudAiplatformV1SchemaPredictionResultError
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param array[]
   */
  public function setInstance($instance)
  {
    $this->instance = $instance;
  }
  /**
   * @return array[]
   */
  public function getInstance()
  {
    return $this->instance;
  }
  /**
   * @param string
   */
  public function setKey($key)
  {
    $this->key = $key;
  }
  /**
   * @return string
   */
  public function getKey()
  {
    return $this->key;
  }
  /**
   * @param array
   */
  public function setPrediction($prediction)
  {
    $this->prediction = $prediction;
  }
  /**
   * @return array
   */
  public function getPrediction()
  {
    return $this->prediction;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1SchemaPredictionResult::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1SchemaPredictionResult');
