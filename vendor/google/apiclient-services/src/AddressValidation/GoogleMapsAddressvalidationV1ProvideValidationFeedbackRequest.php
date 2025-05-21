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

namespace Google\Service\AddressValidation;

class GoogleMapsAddressvalidationV1ProvideValidationFeedbackRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $conclusion;
  /**
   * @var string
   */
  public $responseId;

  /**
   * @param string
   */
  public function setConclusion($conclusion)
  {
    $this->conclusion = $conclusion;
  }
  /**
   * @return string
   */
  public function getConclusion()
  {
    return $this->conclusion;
  }
  /**
   * @param string
   */
  public function setResponseId($responseId)
  {
    $this->responseId = $responseId;
  }
  /**
   * @return string
   */
  public function getResponseId()
  {
    return $this->responseId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleMapsAddressvalidationV1ProvideValidationFeedbackRequest::class, 'Google_Service_AddressValidation_GoogleMapsAddressvalidationV1ProvideValidationFeedbackRequest');
