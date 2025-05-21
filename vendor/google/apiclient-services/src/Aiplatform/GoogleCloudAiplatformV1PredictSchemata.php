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

class GoogleCloudAiplatformV1PredictSchemata extends \Google\Model
{
  /**
   * @var string
   */
  public $instanceSchemaUri;
  /**
   * @var string
   */
  public $parametersSchemaUri;
  /**
   * @var string
   */
  public $predictionSchemaUri;

  /**
   * @param string
   */
  public function setInstanceSchemaUri($instanceSchemaUri)
  {
    $this->instanceSchemaUri = $instanceSchemaUri;
  }
  /**
   * @return string
   */
  public function getInstanceSchemaUri()
  {
    return $this->instanceSchemaUri;
  }
  /**
   * @param string
   */
  public function setParametersSchemaUri($parametersSchemaUri)
  {
    $this->parametersSchemaUri = $parametersSchemaUri;
  }
  /**
   * @return string
   */
  public function getParametersSchemaUri()
  {
    return $this->parametersSchemaUri;
  }
  /**
   * @param string
   */
  public function setPredictionSchemaUri($predictionSchemaUri)
  {
    $this->predictionSchemaUri = $predictionSchemaUri;
  }
  /**
   * @return string
   */
  public function getPredictionSchemaUri()
  {
    return $this->predictionSchemaUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PredictSchemata::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PredictSchemata');
