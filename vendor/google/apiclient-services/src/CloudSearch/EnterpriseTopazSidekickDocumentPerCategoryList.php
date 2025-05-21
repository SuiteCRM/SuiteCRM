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

namespace Google\Service\CloudSearch;

class EnterpriseTopazSidekickDocumentPerCategoryList extends \Google\Collection
{
  protected $collection_key = 'documents';
  protected $documentsType = EnterpriseTopazSidekickDocumentPerCategoryListDocumentPerCategoryListEntry::class;
  protected $documentsDataType = 'array';
  /**
   * @var string
   */
  public $helpMessage;
  /**
   * @var string
   */
  public $listType;
  /**
   * @var string
   */
  public $listTypeDescription;
  /**
   * @var string
   */
  public $responseMessage;

  /**
   * @param EnterpriseTopazSidekickDocumentPerCategoryListDocumentPerCategoryListEntry[]
   */
  public function setDocuments($documents)
  {
    $this->documents = $documents;
  }
  /**
   * @return EnterpriseTopazSidekickDocumentPerCategoryListDocumentPerCategoryListEntry[]
   */
  public function getDocuments()
  {
    return $this->documents;
  }
  /**
   * @param string
   */
  public function setHelpMessage($helpMessage)
  {
    $this->helpMessage = $helpMessage;
  }
  /**
   * @return string
   */
  public function getHelpMessage()
  {
    return $this->helpMessage;
  }
  /**
   * @param string
   */
  public function setListType($listType)
  {
    $this->listType = $listType;
  }
  /**
   * @return string
   */
  public function getListType()
  {
    return $this->listType;
  }
  /**
   * @param string
   */
  public function setListTypeDescription($listTypeDescription)
  {
    $this->listTypeDescription = $listTypeDescription;
  }
  /**
   * @return string
   */
  public function getListTypeDescription()
  {
    return $this->listTypeDescription;
  }
  /**
   * @param string
   */
  public function setResponseMessage($responseMessage)
  {
    $this->responseMessage = $responseMessage;
  }
  /**
   * @return string
   */
  public function getResponseMessage()
  {
    return $this->responseMessage;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnterpriseTopazSidekickDocumentPerCategoryList::class, 'Google_Service_CloudSearch_EnterpriseTopazSidekickDocumentPerCategoryList');
