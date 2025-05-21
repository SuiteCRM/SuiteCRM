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

class GoogleChecksReportV1alphaAnalyzeUploadRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $appBinaryFileType;
  /**
   * @var string
   */
  public $codeReferenceId;

  /**
   * @param string
   */
  public function setAppBinaryFileType($appBinaryFileType)
  {
    $this->appBinaryFileType = $appBinaryFileType;
  }
  /**
   * @return string
   */
  public function getAppBinaryFileType()
  {
    return $this->appBinaryFileType;
  }
  /**
   * @param string
   */
  public function setCodeReferenceId($codeReferenceId)
  {
    $this->codeReferenceId = $codeReferenceId;
  }
  /**
   * @return string
   */
  public function getCodeReferenceId()
  {
    return $this->codeReferenceId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChecksReportV1alphaAnalyzeUploadRequest::class, 'Google_Service_ChecksService_GoogleChecksReportV1alphaAnalyzeUploadRequest');
