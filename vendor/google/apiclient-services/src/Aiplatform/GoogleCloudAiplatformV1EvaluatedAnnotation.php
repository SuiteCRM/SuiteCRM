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

class GoogleCloudAiplatformV1EvaluatedAnnotation extends \Google\Collection
{
  protected $collection_key = 'predictions';
  /**
   * @var array
   */
  public $dataItemPayload;
  protected $errorAnalysisAnnotationsType = GoogleCloudAiplatformV1ErrorAnalysisAnnotation::class;
  protected $errorAnalysisAnnotationsDataType = 'array';
  /**
   * @var string
   */
  public $evaluatedDataItemViewId;
  protected $explanationsType = GoogleCloudAiplatformV1EvaluatedAnnotationExplanation::class;
  protected $explanationsDataType = 'array';
  /**
   * @var array[]
   */
  public $groundTruths;
  /**
   * @var array[]
   */
  public $predictions;
  /**
   * @var string
   */
  public $type;

  /**
   * @param array
   */
  public function setDataItemPayload($dataItemPayload)
  {
    $this->dataItemPayload = $dataItemPayload;
  }
  /**
   * @return array
   */
  public function getDataItemPayload()
  {
    return $this->dataItemPayload;
  }
  /**
   * @param GoogleCloudAiplatformV1ErrorAnalysisAnnotation[]
   */
  public function setErrorAnalysisAnnotations($errorAnalysisAnnotations)
  {
    $this->errorAnalysisAnnotations = $errorAnalysisAnnotations;
  }
  /**
   * @return GoogleCloudAiplatformV1ErrorAnalysisAnnotation[]
   */
  public function getErrorAnalysisAnnotations()
  {
    return $this->errorAnalysisAnnotations;
  }
  /**
   * @param string
   */
  public function setEvaluatedDataItemViewId($evaluatedDataItemViewId)
  {
    $this->evaluatedDataItemViewId = $evaluatedDataItemViewId;
  }
  /**
   * @return string
   */
  public function getEvaluatedDataItemViewId()
  {
    return $this->evaluatedDataItemViewId;
  }
  /**
   * @param GoogleCloudAiplatformV1EvaluatedAnnotationExplanation[]
   */
  public function setExplanations($explanations)
  {
    $this->explanations = $explanations;
  }
  /**
   * @return GoogleCloudAiplatformV1EvaluatedAnnotationExplanation[]
   */
  public function getExplanations()
  {
    return $this->explanations;
  }
  /**
   * @param array[]
   */
  public function setGroundTruths($groundTruths)
  {
    $this->groundTruths = $groundTruths;
  }
  /**
   * @return array[]
   */
  public function getGroundTruths()
  {
    return $this->groundTruths;
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
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1EvaluatedAnnotation::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1EvaluatedAnnotation');
