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

namespace Google\Service\CloudHealthcare;

class DicomStoreMetrics extends \Google\Model
{
  /**
   * @var string
   */
  public $blobStorageSizeBytes;
  /**
   * @var string
   */
  public $instanceCount;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $seriesCount;
  /**
   * @var string
   */
  public $structuredStorageSizeBytes;
  /**
   * @var string
   */
  public $studyCount;

  /**
   * @param string
   */
  public function setBlobStorageSizeBytes($blobStorageSizeBytes)
  {
    $this->blobStorageSizeBytes = $blobStorageSizeBytes;
  }
  /**
   * @return string
   */
  public function getBlobStorageSizeBytes()
  {
    return $this->blobStorageSizeBytes;
  }
  /**
   * @param string
   */
  public function setInstanceCount($instanceCount)
  {
    $this->instanceCount = $instanceCount;
  }
  /**
   * @return string
   */
  public function getInstanceCount()
  {
    return $this->instanceCount;
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
  public function setSeriesCount($seriesCount)
  {
    $this->seriesCount = $seriesCount;
  }
  /**
   * @return string
   */
  public function getSeriesCount()
  {
    return $this->seriesCount;
  }
  /**
   * @param string
   */
  public function setStructuredStorageSizeBytes($structuredStorageSizeBytes)
  {
    $this->structuredStorageSizeBytes = $structuredStorageSizeBytes;
  }
  /**
   * @return string
   */
  public function getStructuredStorageSizeBytes()
  {
    return $this->structuredStorageSizeBytes;
  }
  /**
   * @param string
   */
  public function setStudyCount($studyCount)
  {
    $this->studyCount = $studyCount;
  }
  /**
   * @return string
   */
  public function getStudyCount()
  {
    return $this->studyCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DicomStoreMetrics::class, 'Google_Service_CloudHealthcare_DicomStoreMetrics');
