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

class GoogleCloudAiplatformV1BatchReadFeatureValuesRequest extends \Google\Collection
{
  protected $collection_key = 'passThroughFields';
  protected $bigqueryReadInstancesType = GoogleCloudAiplatformV1BigQuerySource::class;
  protected $bigqueryReadInstancesDataType = '';
  protected $csvReadInstancesType = GoogleCloudAiplatformV1CsvSource::class;
  protected $csvReadInstancesDataType = '';
  protected $destinationType = GoogleCloudAiplatformV1FeatureValueDestination::class;
  protected $destinationDataType = '';
  protected $entityTypeSpecsType = GoogleCloudAiplatformV1BatchReadFeatureValuesRequestEntityTypeSpec::class;
  protected $entityTypeSpecsDataType = 'array';
  protected $passThroughFieldsType = GoogleCloudAiplatformV1BatchReadFeatureValuesRequestPassThroughField::class;
  protected $passThroughFieldsDataType = 'array';
  /**
   * @var string
   */
  public $startTime;

  /**
   * @param GoogleCloudAiplatformV1BigQuerySource
   */
  public function setBigqueryReadInstances(GoogleCloudAiplatformV1BigQuerySource $bigqueryReadInstances)
  {
    $this->bigqueryReadInstances = $bigqueryReadInstances;
  }
  /**
   * @return GoogleCloudAiplatformV1BigQuerySource
   */
  public function getBigqueryReadInstances()
  {
    return $this->bigqueryReadInstances;
  }
  /**
   * @param GoogleCloudAiplatformV1CsvSource
   */
  public function setCsvReadInstances(GoogleCloudAiplatformV1CsvSource $csvReadInstances)
  {
    $this->csvReadInstances = $csvReadInstances;
  }
  /**
   * @return GoogleCloudAiplatformV1CsvSource
   */
  public function getCsvReadInstances()
  {
    return $this->csvReadInstances;
  }
  /**
   * @param GoogleCloudAiplatformV1FeatureValueDestination
   */
  public function setDestination(GoogleCloudAiplatformV1FeatureValueDestination $destination)
  {
    $this->destination = $destination;
  }
  /**
   * @return GoogleCloudAiplatformV1FeatureValueDestination
   */
  public function getDestination()
  {
    return $this->destination;
  }
  /**
   * @param GoogleCloudAiplatformV1BatchReadFeatureValuesRequestEntityTypeSpec[]
   */
  public function setEntityTypeSpecs($entityTypeSpecs)
  {
    $this->entityTypeSpecs = $entityTypeSpecs;
  }
  /**
   * @return GoogleCloudAiplatformV1BatchReadFeatureValuesRequestEntityTypeSpec[]
   */
  public function getEntityTypeSpecs()
  {
    return $this->entityTypeSpecs;
  }
  /**
   * @param GoogleCloudAiplatformV1BatchReadFeatureValuesRequestPassThroughField[]
   */
  public function setPassThroughFields($passThroughFields)
  {
    $this->passThroughFields = $passThroughFields;
  }
  /**
   * @return GoogleCloudAiplatformV1BatchReadFeatureValuesRequestPassThroughField[]
   */
  public function getPassThroughFields()
  {
    return $this->passThroughFields;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1BatchReadFeatureValuesRequest::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1BatchReadFeatureValuesRequest');
