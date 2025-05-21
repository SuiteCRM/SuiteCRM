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

class GoogleCloudAiplatformV1ModelEvaluation extends \Google\Collection
{
  protected $collection_key = 'sliceDimensions';
  /**
   * @var string
   */
  public $annotationSchemaUri;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $dataItemSchemaUri;
  /**
   * @var string
   */
  public $displayName;
  protected $explanationSpecsType = GoogleCloudAiplatformV1ModelEvaluationModelEvaluationExplanationSpec::class;
  protected $explanationSpecsDataType = 'array';
  /**
   * @var array
   */
  public $metadata;
  /**
   * @var array
   */
  public $metrics;
  /**
   * @var string
   */
  public $metricsSchemaUri;
  protected $modelExplanationType = GoogleCloudAiplatformV1ModelExplanation::class;
  protected $modelExplanationDataType = '';
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $sliceDimensions;

  /**
   * @param string
   */
  public function setAnnotationSchemaUri($annotationSchemaUri)
  {
    $this->annotationSchemaUri = $annotationSchemaUri;
  }
  /**
   * @return string
   */
  public function getAnnotationSchemaUri()
  {
    return $this->annotationSchemaUri;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDataItemSchemaUri($dataItemSchemaUri)
  {
    $this->dataItemSchemaUri = $dataItemSchemaUri;
  }
  /**
   * @return string
   */
  public function getDataItemSchemaUri()
  {
    return $this->dataItemSchemaUri;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelEvaluationModelEvaluationExplanationSpec[]
   */
  public function setExplanationSpecs($explanationSpecs)
  {
    $this->explanationSpecs = $explanationSpecs;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelEvaluationModelEvaluationExplanationSpec[]
   */
  public function getExplanationSpecs()
  {
    return $this->explanationSpecs;
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
   * @param array
   */
  public function setMetrics($metrics)
  {
    $this->metrics = $metrics;
  }
  /**
   * @return array
   */
  public function getMetrics()
  {
    return $this->metrics;
  }
  /**
   * @param string
   */
  public function setMetricsSchemaUri($metricsSchemaUri)
  {
    $this->metricsSchemaUri = $metricsSchemaUri;
  }
  /**
   * @return string
   */
  public function getMetricsSchemaUri()
  {
    return $this->metricsSchemaUri;
  }
  /**
   * @param GoogleCloudAiplatformV1ModelExplanation
   */
  public function setModelExplanation(GoogleCloudAiplatformV1ModelExplanation $modelExplanation)
  {
    $this->modelExplanation = $modelExplanation;
  }
  /**
   * @return GoogleCloudAiplatformV1ModelExplanation
   */
  public function getModelExplanation()
  {
    return $this->modelExplanation;
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
   * @param string[]
   */
  public function setSliceDimensions($sliceDimensions)
  {
    $this->sliceDimensions = $sliceDimensions;
  }
  /**
   * @return string[]
   */
  public function getSliceDimensions()
  {
    return $this->sliceDimensions;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ModelEvaluation::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ModelEvaluation');
