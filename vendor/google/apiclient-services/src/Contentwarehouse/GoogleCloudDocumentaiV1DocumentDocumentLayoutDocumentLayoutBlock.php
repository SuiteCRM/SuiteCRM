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

namespace Google\Service\Contentwarehouse;

class GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlock extends \Google\Model
{
  /**
   * @var string
   */
  public $blockId;
  protected $listBlockType = GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutListBlock::class;
  protected $listBlockDataType = '';
  protected $pageSpanType = GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutPageSpan::class;
  protected $pageSpanDataType = '';
  protected $tableBlockType = GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutTableBlock::class;
  protected $tableBlockDataType = '';
  protected $textBlockType = GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutTextBlock::class;
  protected $textBlockDataType = '';

  /**
   * @param string
   */
  public function setBlockId($blockId)
  {
    $this->blockId = $blockId;
  }
  /**
   * @return string
   */
  public function getBlockId()
  {
    return $this->blockId;
  }
  /**
   * @param GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutListBlock
   */
  public function setListBlock(GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutListBlock $listBlock)
  {
    $this->listBlock = $listBlock;
  }
  /**
   * @return GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutListBlock
   */
  public function getListBlock()
  {
    return $this->listBlock;
  }
  /**
   * @param GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutPageSpan
   */
  public function setPageSpan(GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutPageSpan $pageSpan)
  {
    $this->pageSpan = $pageSpan;
  }
  /**
   * @return GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutPageSpan
   */
  public function getPageSpan()
  {
    return $this->pageSpan;
  }
  /**
   * @param GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutTableBlock
   */
  public function setTableBlock(GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutTableBlock $tableBlock)
  {
    $this->tableBlock = $tableBlock;
  }
  /**
   * @return GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutTableBlock
   */
  public function getTableBlock()
  {
    return $this->tableBlock;
  }
  /**
   * @param GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutTextBlock
   */
  public function setTextBlock(GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutTextBlock $textBlock)
  {
    $this->textBlock = $textBlock;
  }
  /**
   * @return GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlockLayoutTextBlock
   */
  public function getTextBlock()
  {
    return $this->textBlock;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlock::class, 'Google_Service_Contentwarehouse_GoogleCloudDocumentaiV1DocumentDocumentLayoutDocumentLayoutBlock');
