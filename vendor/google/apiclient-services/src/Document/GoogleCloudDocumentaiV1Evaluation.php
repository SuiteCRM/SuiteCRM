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

namespace Google\Service\Document;

class GoogleCloudDocumentaiV1Evaluation extends \Google\Model
{
  protected $allEntitiesMetricsType = GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics::class;
  protected $allEntitiesMetricsDataType = '';
  /**
   * @var string
   */
  public $createTime;
  protected $documentCountersType = GoogleCloudDocumentaiV1EvaluationCounters::class;
  protected $documentCountersDataType = '';
  protected $entityMetricsType = GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics::class;
  protected $entityMetricsDataType = 'map';
  /**
   * @var string
   */
  public $kmsKeyName;
  /**
   * @var string
   */
  public $kmsKeyVersionName;
  /**
   * @var string
   */
  public $name;

  /**
   * @param GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics
   */
  public function setAllEntitiesMetrics(GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics $allEntitiesMetrics)
  {
    $this->allEntitiesMetrics = $allEntitiesMetrics;
  }
  /**
   * @return GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics
   */
  public function getAllEntitiesMetrics()
  {
    return $this->allEntitiesMetrics;
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
   * @param GoogleCloudDocumentaiV1EvaluationCounters
   */
  public function setDocumentCounters(GoogleCloudDocumentaiV1EvaluationCounters $documentCounters)
  {
    $this->documentCounters = $documentCounters;
  }
  /**
   * @return GoogleCloudDocumentaiV1EvaluationCounters
   */
  public function getDocumentCounters()
  {
    return $this->documentCounters;
  }
  /**
   * @param GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics[]
   */
  public function setEntityMetrics($entityMetrics)
  {
    $this->entityMetrics = $entityMetrics;
  }
  /**
   * @return GoogleCloudDocumentaiV1EvaluationMultiConfidenceMetrics[]
   */
  public function getEntityMetrics()
  {
    return $this->entityMetrics;
  }
  /**
   * @param string
   */
  public function setKmsKeyName($kmsKeyName)
  {
    $this->kmsKeyName = $kmsKeyName;
  }
  /**
   * @return string
   */
  public function getKmsKeyName()
  {
    return $this->kmsKeyName;
  }
  /**
   * @param string
   */
  public function setKmsKeyVersionName($kmsKeyVersionName)
  {
    $this->kmsKeyVersionName = $kmsKeyVersionName;
  }
  /**
   * @return string
   */
  public function getKmsKeyVersionName()
  {
    return $this->kmsKeyVersionName;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDocumentaiV1Evaluation::class, 'Google_Service_Document_GoogleCloudDocumentaiV1Evaluation');
