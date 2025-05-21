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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ErrorsErrorCode extends \Google\Model
{
  /**
   * @var string
   */
  public $authenticationError;
  /**
   * @var string
   */
  public $authorizationError;
  /**
   * @var string
   */
  public $customColumnError;
  /**
   * @var string
   */
  public $dateError;
  /**
   * @var string
   */
  public $dateRangeError;
  /**
   * @var string
   */
  public $distinctError;
  /**
   * @var string
   */
  public $headerError;
  /**
   * @var string
   */
  public $internalError;
  /**
   * @var string
   */
  public $invalidParameterError;
  /**
   * @var string
   */
  public $queryError;
  /**
   * @var string
   */
  public $quotaError;
  /**
   * @var string
   */
  public $requestError;
  /**
   * @var string
   */
  public $sizeLimitError;

  /**
   * @param string
   */
  public function setAuthenticationError($authenticationError)
  {
    $this->authenticationError = $authenticationError;
  }
  /**
   * @return string
   */
  public function getAuthenticationError()
  {
    return $this->authenticationError;
  }
  /**
   * @param string
   */
  public function setAuthorizationError($authorizationError)
  {
    $this->authorizationError = $authorizationError;
  }
  /**
   * @return string
   */
  public function getAuthorizationError()
  {
    return $this->authorizationError;
  }
  /**
   * @param string
   */
  public function setCustomColumnError($customColumnError)
  {
    $this->customColumnError = $customColumnError;
  }
  /**
   * @return string
   */
  public function getCustomColumnError()
  {
    return $this->customColumnError;
  }
  /**
   * @param string
   */
  public function setDateError($dateError)
  {
    $this->dateError = $dateError;
  }
  /**
   * @return string
   */
  public function getDateError()
  {
    return $this->dateError;
  }
  /**
   * @param string
   */
  public function setDateRangeError($dateRangeError)
  {
    $this->dateRangeError = $dateRangeError;
  }
  /**
   * @return string
   */
  public function getDateRangeError()
  {
    return $this->dateRangeError;
  }
  /**
   * @param string
   */
  public function setDistinctError($distinctError)
  {
    $this->distinctError = $distinctError;
  }
  /**
   * @return string
   */
  public function getDistinctError()
  {
    return $this->distinctError;
  }
  /**
   * @param string
   */
  public function setHeaderError($headerError)
  {
    $this->headerError = $headerError;
  }
  /**
   * @return string
   */
  public function getHeaderError()
  {
    return $this->headerError;
  }
  /**
   * @param string
   */
  public function setInternalError($internalError)
  {
    $this->internalError = $internalError;
  }
  /**
   * @return string
   */
  public function getInternalError()
  {
    return $this->internalError;
  }
  /**
   * @param string
   */
  public function setInvalidParameterError($invalidParameterError)
  {
    $this->invalidParameterError = $invalidParameterError;
  }
  /**
   * @return string
   */
  public function getInvalidParameterError()
  {
    return $this->invalidParameterError;
  }
  /**
   * @param string
   */
  public function setQueryError($queryError)
  {
    $this->queryError = $queryError;
  }
  /**
   * @return string
   */
  public function getQueryError()
  {
    return $this->queryError;
  }
  /**
   * @param string
   */
  public function setQuotaError($quotaError)
  {
    $this->quotaError = $quotaError;
  }
  /**
   * @return string
   */
  public function getQuotaError()
  {
    return $this->quotaError;
  }
  /**
   * @param string
   */
  public function setRequestError($requestError)
  {
    $this->requestError = $requestError;
  }
  /**
   * @return string
   */
  public function getRequestError()
  {
    return $this->requestError;
  }
  /**
   * @param string
   */
  public function setSizeLimitError($sizeLimitError)
  {
    $this->sizeLimitError = $sizeLimitError;
  }
  /**
   * @return string
   */
  public function getSizeLimitError()
  {
    return $this->sizeLimitError;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ErrorsErrorCode::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ErrorsErrorCode');
