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

class GoogleCloudDocumentaiV1TrainProcessorVersionRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $baseProcessorVersion;
  protected $customDocumentExtractionOptionsType = GoogleCloudDocumentaiV1TrainProcessorVersionRequestCustomDocumentExtractionOptions::class;
  protected $customDocumentExtractionOptionsDataType = '';
  protected $documentSchemaType = GoogleCloudDocumentaiV1DocumentSchema::class;
  protected $documentSchemaDataType = '';
  protected $foundationModelTuningOptionsType = GoogleCloudDocumentaiV1TrainProcessorVersionRequestFoundationModelTuningOptions::class;
  protected $foundationModelTuningOptionsDataType = '';
  protected $inputDataType = GoogleCloudDocumentaiV1TrainProcessorVersionRequestInputData::class;
  protected $inputDataDataType = '';
  protected $processorVersionType = GoogleCloudDocumentaiV1ProcessorVersion::class;
  protected $processorVersionDataType = '';

  /**
   * @param string
   */
  public function setBaseProcessorVersion($baseProcessorVersion)
  {
    $this->baseProcessorVersion = $baseProcessorVersion;
  }
  /**
   * @return string
   */
  public function getBaseProcessorVersion()
  {
    return $this->baseProcessorVersion;
  }
  /**
   * @param GoogleCloudDocumentaiV1TrainProcessorVersionRequestCustomDocumentExtractionOptions
   */
  public function setCustomDocumentExtractionOptions(GoogleCloudDocumentaiV1TrainProcessorVersionRequestCustomDocumentExtractionOptions $customDocumentExtractionOptions)
  {
    $this->customDocumentExtractionOptions = $customDocumentExtractionOptions;
  }
  /**
   * @return GoogleCloudDocumentaiV1TrainProcessorVersionRequestCustomDocumentExtractionOptions
   */
  public function getCustomDocumentExtractionOptions()
  {
    return $this->customDocumentExtractionOptions;
  }
  /**
   * @param GoogleCloudDocumentaiV1DocumentSchema
   */
  public function setDocumentSchema(GoogleCloudDocumentaiV1DocumentSchema $documentSchema)
  {
    $this->documentSchema = $documentSchema;
  }
  /**
   * @return GoogleCloudDocumentaiV1DocumentSchema
   */
  public function getDocumentSchema()
  {
    return $this->documentSchema;
  }
  /**
   * @param GoogleCloudDocumentaiV1TrainProcessorVersionRequestFoundationModelTuningOptions
   */
  public function setFoundationModelTuningOptions(GoogleCloudDocumentaiV1TrainProcessorVersionRequestFoundationModelTuningOptions $foundationModelTuningOptions)
  {
    $this->foundationModelTuningOptions = $foundationModelTuningOptions;
  }
  /**
   * @return GoogleCloudDocumentaiV1TrainProcessorVersionRequestFoundationModelTuningOptions
   */
  public function getFoundationModelTuningOptions()
  {
    return $this->foundationModelTuningOptions;
  }
  /**
   * @param GoogleCloudDocumentaiV1TrainProcessorVersionRequestInputData
   */
  public function setInputData(GoogleCloudDocumentaiV1TrainProcessorVersionRequestInputData $inputData)
  {
    $this->inputData = $inputData;
  }
  /**
   * @return GoogleCloudDocumentaiV1TrainProcessorVersionRequestInputData
   */
  public function getInputData()
  {
    return $this->inputData;
  }
  /**
   * @param GoogleCloudDocumentaiV1ProcessorVersion
   */
  public function setProcessorVersion(GoogleCloudDocumentaiV1ProcessorVersion $processorVersion)
  {
    $this->processorVersion = $processorVersion;
  }
  /**
   * @return GoogleCloudDocumentaiV1ProcessorVersion
   */
  public function getProcessorVersion()
  {
    return $this->processorVersion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDocumentaiV1TrainProcessorVersionRequest::class, 'Google_Service_Document_GoogleCloudDocumentaiV1TrainProcessorVersionRequest');
