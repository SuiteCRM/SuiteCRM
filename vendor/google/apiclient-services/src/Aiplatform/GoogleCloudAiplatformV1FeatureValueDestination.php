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

class GoogleCloudAiplatformV1FeatureValueDestination extends \Google\Model
{
  protected $bigqueryDestinationType = GoogleCloudAiplatformV1BigQueryDestination::class;
  protected $bigqueryDestinationDataType = '';
  protected $csvDestinationType = GoogleCloudAiplatformV1CsvDestination::class;
  protected $csvDestinationDataType = '';
  protected $tfrecordDestinationType = GoogleCloudAiplatformV1TFRecordDestination::class;
  protected $tfrecordDestinationDataType = '';

  /**
   * @param GoogleCloudAiplatformV1BigQueryDestination
   */
  public function setBigqueryDestination(GoogleCloudAiplatformV1BigQueryDestination $bigqueryDestination)
  {
    $this->bigqueryDestination = $bigqueryDestination;
  }
  /**
   * @return GoogleCloudAiplatformV1BigQueryDestination
   */
  public function getBigqueryDestination()
  {
    return $this->bigqueryDestination;
  }
  /**
   * @param GoogleCloudAiplatformV1CsvDestination
   */
  public function setCsvDestination(GoogleCloudAiplatformV1CsvDestination $csvDestination)
  {
    $this->csvDestination = $csvDestination;
  }
  /**
   * @return GoogleCloudAiplatformV1CsvDestination
   */
  public function getCsvDestination()
  {
    return $this->csvDestination;
  }
  /**
   * @param GoogleCloudAiplatformV1TFRecordDestination
   */
  public function setTfrecordDestination(GoogleCloudAiplatformV1TFRecordDestination $tfrecordDestination)
  {
    $this->tfrecordDestination = $tfrecordDestination;
  }
  /**
   * @return GoogleCloudAiplatformV1TFRecordDestination
   */
  public function getTfrecordDestination()
  {
    return $this->tfrecordDestination;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1FeatureValueDestination::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1FeatureValueDestination');
