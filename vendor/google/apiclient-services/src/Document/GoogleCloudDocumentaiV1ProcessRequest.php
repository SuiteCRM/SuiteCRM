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

class GoogleCloudDocumentaiV1ProcessRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $fieldMask;
  protected $gcsDocumentType = GoogleCloudDocumentaiV1GcsDocument::class;
  protected $gcsDocumentDataType = '';
  /**
   * @var bool
   */
  public $imagelessMode;
  protected $inlineDocumentType = GoogleCloudDocumentaiV1Document::class;
  protected $inlineDocumentDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  protected $processOptionsType = GoogleCloudDocumentaiV1ProcessOptions::class;
  protected $processOptionsDataType = '';
  protected $rawDocumentType = GoogleCloudDocumentaiV1RawDocument::class;
  protected $rawDocumentDataType = '';
  /**
   * @var bool
   */
  public $skipHumanReview;

  /**
   * @param string
   */
  public function setFieldMask($fieldMask)
  {
    $this->fieldMask = $fieldMask;
  }
  /**
   * @return string
   */
  public function getFieldMask()
  {
    return $this->fieldMask;
  }
  /**
   * @param GoogleCloudDocumentaiV1GcsDocument
   */
  public function setGcsDocument(GoogleCloudDocumentaiV1GcsDocument $gcsDocument)
  {
    $this->gcsDocument = $gcsDocument;
  }
  /**
   * @return GoogleCloudDocumentaiV1GcsDocument
   */
  public function getGcsDocument()
  {
    return $this->gcsDocument;
  }
  /**
   * @param bool
   */
  public function setImagelessMode($imagelessMode)
  {
    $this->imagelessMode = $imagelessMode;
  }
  /**
   * @return bool
   */
  public function getImagelessMode()
  {
    return $this->imagelessMode;
  }
  /**
   * @param GoogleCloudDocumentaiV1Document
   */
  public function setInlineDocument(GoogleCloudDocumentaiV1Document $inlineDocument)
  {
    $this->inlineDocument = $inlineDocument;
  }
  /**
   * @return GoogleCloudDocumentaiV1Document
   */
  public function getInlineDocument()
  {
    return $this->inlineDocument;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param GoogleCloudDocumentaiV1ProcessOptions
   */
  public function setProcessOptions(GoogleCloudDocumentaiV1ProcessOptions $processOptions)
  {
    $this->processOptions = $processOptions;
  }
  /**
   * @return GoogleCloudDocumentaiV1ProcessOptions
   */
  public function getProcessOptions()
  {
    return $this->processOptions;
  }
  /**
   * @param GoogleCloudDocumentaiV1RawDocument
   */
  public function setRawDocument(GoogleCloudDocumentaiV1RawDocument $rawDocument)
  {
    $this->rawDocument = $rawDocument;
  }
  /**
   * @return GoogleCloudDocumentaiV1RawDocument
   */
  public function getRawDocument()
  {
    return $this->rawDocument;
  }
  /**
   * @param bool
   */
  public function setSkipHumanReview($skipHumanReview)
  {
    $this->skipHumanReview = $skipHumanReview;
  }
  /**
   * @return bool
   */
  public function getSkipHumanReview()
  {
    return $this->skipHumanReview;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDocumentaiV1ProcessRequest::class, 'Google_Service_Document_GoogleCloudDocumentaiV1ProcessRequest');
