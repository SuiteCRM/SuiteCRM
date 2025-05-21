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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsMetadataDownloadStats extends \Google\Collection
{
  protected $collection_key = 'fileNames';
  /**
   * @var string[]
   */
  public $fileNames;
  /**
   * @var int
   */
  public $processedObjectCount;
  /**
   * @var int
   */
  public $successfulDownloadCount;
  /**
   * @var int
   */
  public $totalFilesWritten;

  /**
   * @param string[]
   */
  public function setFileNames($fileNames)
  {
    $this->fileNames = $fileNames;
  }
  /**
   * @return string[]
   */
  public function getFileNames()
  {
    return $this->fileNames;
  }
  /**
   * @param int
   */
  public function setProcessedObjectCount($processedObjectCount)
  {
    $this->processedObjectCount = $processedObjectCount;
  }
  /**
   * @return int
   */
  public function getProcessedObjectCount()
  {
    return $this->processedObjectCount;
  }
  /**
   * @param int
   */
  public function setSuccessfulDownloadCount($successfulDownloadCount)
  {
    $this->successfulDownloadCount = $successfulDownloadCount;
  }
  /**
   * @return int
   */
  public function getSuccessfulDownloadCount()
  {
    return $this->successfulDownloadCount;
  }
  /**
   * @param int
   */
  public function setTotalFilesWritten($totalFilesWritten)
  {
    $this->totalFilesWritten = $totalFilesWritten;
  }
  /**
   * @return int
   */
  public function getTotalFilesWritten()
  {
    return $this->totalFilesWritten;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsMetadataDownloadStats::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsMetadataDownloadStats');
