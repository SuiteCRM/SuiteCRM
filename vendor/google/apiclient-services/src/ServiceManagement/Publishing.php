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

namespace Google\Service\ServiceManagement;

class Publishing extends \Google\Collection
{
  protected $collection_key = 'methodSettings';
  /**
   * @var string
   */
  public $apiShortName;
  /**
   * @var string[]
   */
  public $codeownerGithubTeams;
  /**
   * @var string
   */
  public $docTagPrefix;
  /**
   * @var string
   */
  public $documentationUri;
  /**
   * @var string
   */
  public $githubLabel;
  protected $librarySettingsType = ClientLibrarySettings::class;
  protected $librarySettingsDataType = 'array';
  protected $methodSettingsType = MethodSettings::class;
  protected $methodSettingsDataType = 'array';
  /**
   * @var string
   */
  public $newIssueUri;
  /**
   * @var string
   */
  public $organization;
  /**
   * @var string
   */
  public $protoReferenceDocumentationUri;
  /**
   * @var string
   */
  public $restReferenceDocumentationUri;

  /**
   * @param string
   */
  public function setApiShortName($apiShortName)
  {
    $this->apiShortName = $apiShortName;
  }
  /**
   * @return string
   */
  public function getApiShortName()
  {
    return $this->apiShortName;
  }
  /**
   * @param string[]
   */
  public function setCodeownerGithubTeams($codeownerGithubTeams)
  {
    $this->codeownerGithubTeams = $codeownerGithubTeams;
  }
  /**
   * @return string[]
   */
  public function getCodeownerGithubTeams()
  {
    return $this->codeownerGithubTeams;
  }
  /**
   * @param string
   */
  public function setDocTagPrefix($docTagPrefix)
  {
    $this->docTagPrefix = $docTagPrefix;
  }
  /**
   * @return string
   */
  public function getDocTagPrefix()
  {
    return $this->docTagPrefix;
  }
  /**
   * @param string
   */
  public function setDocumentationUri($documentationUri)
  {
    $this->documentationUri = $documentationUri;
  }
  /**
   * @return string
   */
  public function getDocumentationUri()
  {
    return $this->documentationUri;
  }
  /**
   * @param string
   */
  public function setGithubLabel($githubLabel)
  {
    $this->githubLabel = $githubLabel;
  }
  /**
   * @return string
   */
  public function getGithubLabel()
  {
    return $this->githubLabel;
  }
  /**
   * @param ClientLibrarySettings[]
   */
  public function setLibrarySettings($librarySettings)
  {
    $this->librarySettings = $librarySettings;
  }
  /**
   * @return ClientLibrarySettings[]
   */
  public function getLibrarySettings()
  {
    return $this->librarySettings;
  }
  /**
   * @param MethodSettings[]
   */
  public function setMethodSettings($methodSettings)
  {
    $this->methodSettings = $methodSettings;
  }
  /**
   * @return MethodSettings[]
   */
  public function getMethodSettings()
  {
    return $this->methodSettings;
  }
  /**
   * @param string
   */
  public function setNewIssueUri($newIssueUri)
  {
    $this->newIssueUri = $newIssueUri;
  }
  /**
   * @return string
   */
  public function getNewIssueUri()
  {
    return $this->newIssueUri;
  }
  /**
   * @param string
   */
  public function setOrganization($organization)
  {
    $this->organization = $organization;
  }
  /**
   * @return string
   */
  public function getOrganization()
  {
    return $this->organization;
  }
  /**
   * @param string
   */
  public function setProtoReferenceDocumentationUri($protoReferenceDocumentationUri)
  {
    $this->protoReferenceDocumentationUri = $protoReferenceDocumentationUri;
  }
  /**
   * @return string
   */
  public function getProtoReferenceDocumentationUri()
  {
    return $this->protoReferenceDocumentationUri;
  }
  /**
   * @param string
   */
  public function setRestReferenceDocumentationUri($restReferenceDocumentationUri)
  {
    $this->restReferenceDocumentationUri = $restReferenceDocumentationUri;
  }
  /**
   * @return string
   */
  public function getRestReferenceDocumentationUri()
  {
    return $this->restReferenceDocumentationUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Publishing::class, 'Google_Service_ServiceManagement_Publishing');
