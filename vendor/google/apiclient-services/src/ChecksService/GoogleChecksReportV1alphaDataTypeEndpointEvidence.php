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

namespace Google\Service\ChecksService;

class GoogleChecksReportV1alphaDataTypeEndpointEvidence extends \Google\Collection
{
  protected $collection_key = 'endpointDetails';
  protected $attributedSdksType = GoogleChecksReportV1alphaDataTypeEndpointEvidenceAttributedSdk::class;
  protected $attributedSdksDataType = 'array';
  protected $endpointDetailsType = GoogleChecksReportV1alphaDataTypeEndpointEvidenceEndpointDetails::class;
  protected $endpointDetailsDataType = 'array';
  /**
   * @var string
   */
  public $exfiltratedDataType;

  /**
   * @param GoogleChecksReportV1alphaDataTypeEndpointEvidenceAttributedSdk[]
   */
  public function setAttributedSdks($attributedSdks)
  {
    $this->attributedSdks = $attributedSdks;
  }
  /**
   * @return GoogleChecksReportV1alphaDataTypeEndpointEvidenceAttributedSdk[]
   */
  public function getAttributedSdks()
  {
    return $this->attributedSdks;
  }
  /**
   * @param GoogleChecksReportV1alphaDataTypeEndpointEvidenceEndpointDetails[]
   */
  public function setEndpointDetails($endpointDetails)
  {
    $this->endpointDetails = $endpointDetails;
  }
  /**
   * @return GoogleChecksReportV1alphaDataTypeEndpointEvidenceEndpointDetails[]
   */
  public function getEndpointDetails()
  {
    return $this->endpointDetails;
  }
  /**
   * @param string
   */
  public function setExfiltratedDataType($exfiltratedDataType)
  {
    $this->exfiltratedDataType = $exfiltratedDataType;
  }
  /**
   * @return string
   */
  public function getExfiltratedDataType()
  {
    return $this->exfiltratedDataType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChecksReportV1alphaDataTypeEndpointEvidence::class, 'Google_Service_ChecksService_GoogleChecksReportV1alphaDataTypeEndpointEvidence');
