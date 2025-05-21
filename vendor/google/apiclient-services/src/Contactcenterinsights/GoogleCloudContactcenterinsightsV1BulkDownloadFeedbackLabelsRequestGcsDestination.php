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

class GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequestGcsDestination extends \Google\Model
{
  /**
   * @var bool
   */
  public $addWhitespace;
  /**
   * @var bool
   */
  public $alwaysPrintEmptyFields;
  /**
   * @var string
   */
  public $format;
  /**
   * @var string
   */
  public $objectUri;
  /**
   * @var string
   */
  public $recordsPerFileCount;

  /**
   * @param bool
   */
  public function setAddWhitespace($addWhitespace)
  {
    $this->addWhitespace = $addWhitespace;
  }
  /**
   * @return bool
   */
  public function getAddWhitespace()
  {
    return $this->addWhitespace;
  }
  /**
   * @param bool
   */
  public function setAlwaysPrintEmptyFields($alwaysPrintEmptyFields)
  {
    $this->alwaysPrintEmptyFields = $alwaysPrintEmptyFields;
  }
  /**
   * @return bool
   */
  public function getAlwaysPrintEmptyFields()
  {
    return $this->alwaysPrintEmptyFields;
  }
  /**
   * @param string
   */
  public function setFormat($format)
  {
    $this->format = $format;
  }
  /**
   * @return string
   */
  public function getFormat()
  {
    return $this->format;
  }
  /**
   * @param string
   */
  public function setObjectUri($objectUri)
  {
    $this->objectUri = $objectUri;
  }
  /**
   * @return string
   */
  public function getObjectUri()
  {
    return $this->objectUri;
  }
  /**
   * @param string
   */
  public function setRecordsPerFileCount($recordsPerFileCount)
  {
    $this->recordsPerFileCount = $recordsPerFileCount;
  }
  /**
   * @return string
   */
  public function getRecordsPerFileCount()
  {
    return $this->recordsPerFileCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequestGcsDestination::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1BulkDownloadFeedbackLabelsRequestGcsDestination');
